{- | jev-judge: keep natural-language judgments current for one SpacetimeDB table.

Rows are decoded from the database's own schema (no per-table code), the
chosen columns are sent to a Jev-compatible endpoint in batches, and answers
go to stdout as JSON lines or, with --judgments-db, into the jev module's
@judgment@ table. With --judgments-db, conditions also come from (and can be
added live through) that module's @condition@ table.
-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Monad (forM_, forever, unless, void, when)
import Data.Aeson (FromJSON (..), Value (..), withObject, (.!=), (.:), (.:?))
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.Either (partitionEithers)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Jev qualified
import JevSchema (App (..), Condition (..), Judgment (..), app)
import Options.Applicative
import SpacetimeDB.BSATN.Decoder (runExact)
import SpacetimeDB.Client qualified as S
import SpacetimeDB.Client.Typed (subscribeTable)
import SpacetimeDB.Codegen.Schema (parseModule)
import SpacetimeJev.Batch (Question (Question), kindFromText)
import SpacetimeJev.Dynamic (RowCodec (..), renderKey, rowCodec)
import SpacetimeJev.Judge
import SpacetimeJev.Sink
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, stdout)

data Opts = Opts
  { host :: Text
  , port :: Int
  , db :: Text
  , table :: Text
  , schemaFile :: FilePath
  , keyCol :: Maybe Text
  , fields :: [Text]
  , skipNull :: Bool
  , jevUrl :: Text
  , jevEndpoint :: Text
  , model :: Text
  , batch :: Int
  , maxChars :: Int
  , nWorkers :: Int
  , lingerMs :: Int
  , cap :: Maybe Int
  , conditionsFile :: Maybe FilePath
  , askText :: Maybe Text
  , judgmentsDb :: Maybe Text
  , once :: Bool
  , withRow :: Bool
  }

optsP :: Parser Opts
optsP =
  Opts
    <$> strOption (long "host" <> value "127.0.0.1" <> showDefault <> help "SpacetimeDB host")
    <*> option auto (long "port" <> value 3000 <> showDefault <> help "SpacetimeDB port")
    <*> strOption (long "db" <> metavar "DB" <> help "database holding the source table")
    <*> strOption (long "table" <> metavar "TABLE" <> help "table to judge")
    <*> strOption (long "schema" <> metavar "FILE" <> help "output of `spacetime describe --json DB` (- for stdin)")
    <*> optional (strOption (long "key" <> metavar "COL" <> help "row key column (default: first column)"))
    <*> (maybe [] (T.splitOn "," . T.pack) <$> optional (strOption (long "fields" <> metavar "A,B" <> help "columns to send (default: all) — your pg-jev 'view'")))
    <*> switch (long "skip-null" <> help "don't judge rows whose sent columns are all null")
    <*> strOption (long "jev-url" <> value "https://api.typesafe.ai" <> showDefault <> help "Jev-compatible base URL (Intern-Decision: http://HOST:8100)")
    <*> strOption (long "jev-endpoint" <> value "/v1/systemone" <> showDefault <> help "request path (Intern-Decision: /v1/jev)")
    <*> strOption (long "model" <> value "jev-latest" <> showDefault <> help "model name sent and recorded")
    <*> option auto (long "batch" <> value 20 <> showDefault <> help "rows per request")
    <*> option auto (long "max-chars" <> value 24000 <> showDefault <> help "JSON characters of row data per request")
    <*> option auto (long "workers" <> value 4 <> showDefault <> help "requests in flight (1-2 for a single local GPU)")
    <*> option auto (long "linger-ms" <> value 20 <> showDefault <> help "wait this long to fill a batch")
    <*> optional (option auto (long "max-rows" <> help "spend guard: stop after judging this many rows"))
    <*> optional (strOption (long "conditions" <> metavar "FILE" <> help "JSON list of {id, kind, text, options}"))
    <*> optional (strOption (long "ask" <> metavar "TEXT" <> help "shorthand for one yes/no condition with id 'ask'"))
    <*> optional (strOption (long "judgments-db" <> metavar "DB" <> help "jev module database: write judgments there and read conditions from it"))
    <*> switch (long "once" <> help "judge the current rows, print stats, exit")
    <*> switch (long "with-row" <> help "include the judged columns in JSON-lines output")

data CondJson = CondJson Text Text Text [Text]

instance FromJSON CondJson where
  parseJSON = withObject "condition" $ \o ->
    CondJson <$> o .: "id" <*> o .: "kind" <*> o .: "text" <*> o .:? "options" .!= []

toQuestion :: Text -> Text -> Text -> [Text] -> Either Text Question
toQuestion i k t os = Question i t <$> kindFromText k os

main :: IO ()
main = do
  o <- execParser (info (optsP <**> helper) (fullDesc <> progDesc "Keep Jev judgments current for a SpacetimeDB table"))
  let die' msg = TIO.hPutStrLn stderr ("jev-judge: " <> msg) >> exitFailure
      logE = TIO.hPutStrLn stderr . ("jev-judge: " <>)

  -- Schema -> a decoder for the table, the key column and the projection.
  raw <- if o.schemaFile == "-" then BL.getContents else BL.readFile o.schemaFile
  m <- either (die' . T.pack) pure (parseModule raw)
  codec <- either die' pure (rowCodec m o.table)
  let keyName = fromMaybe (case codec.columns of (c : _) -> c; [] -> "") o.keyCol
      sendCols = if null o.fields then codec.columns else o.fields
  forM_ (keyName : sendCols) $ \c ->
    unless (c `elem` codec.columns) (die' ("no column " <> c <> " in " <> o.table <> " (" <> T.intercalate ", " codec.columns <> ")"))
  let toItem bs = case runExact codec.decodeRow bs of
        Left e -> Left (T.pack (show e))
        Right (Object row) ->
          let k = renderKey (fromMaybe Null (KM.lookup (Key.fromText keyName) row))
              sent = KM.filterWithKey (\c _ -> Key.toText c `elem` sendCols) row
           in Right (k, if o.skipNull && all (== Null) (KM.elems sent) then Nothing else Just (mkItem k (Object sent)))
        Right _ -> Left "row did not decode to an object"
      decodeAll bss = do
        let (errs, oks) = partitionEithers (map toItem bss)
        forM_ (take 3 errs) (logE . ("row decode failed: " <>))
        pure oks

  -- Conditions given on the command line.
  fileConds <- case o.conditionsFile of
    Nothing -> pure []
    Just f -> A.eitherDecodeFileStrict f >>= either (die' . T.pack) pure
  cliQs <-
    either die' pure . traverse (\(CondJson i k t os) -> toQuestion i k t os) $
      fileConds <> maybe [] (\t -> [CondJson "ask" "noul" t []]) o.askText
  when (null cliQs && o.judgmentsDb == Nothing) (die' "nothing to ask: give --ask, --conditions or --judgments-db")

  -- Jev client: TypeSafe, or any wire-compatible server (Intern-Decision serves /v1/jev).
  key <- maybe "local" T.pack <$> lookupEnv "TYPESAFE_API_KEY"
  jc <- Jev.newClientWith (Jev.defaultConfig key) {Jev.baseUrl = o.jevUrl, Jev.endpoint = o.jevEndpoint, Jev.model = o.model}

  -- Intern-Decision takes at most 16 questions per request.
  let local = o.jevEndpoint `elem` ["/v1/jev", "/v1/decisions"]
      batchN = if local then min 16 o.batch else o.batch
  when (batchN /= o.batch) (logE "Intern-Decision takes at most 16 questions per request: using --batch 16")
  let jcfg = JudgeConfig batchN o.maxChars o.nWorkers (o.lingerMs * 1000) o.cap
      onJ = \case
        BatchDone c n tok ms -> logE (c <> ": " <> tshow n <> " rows, " <> tshow tok <> " tokens, " <> tshow ms <> " ms")
        e -> logE (tshow e)
      gates = "source" : maybe [] (const ["conditions", "judgments"]) o.judgmentsDb

  -- The judgment database (optional): sink + live conditions + seeding from existing judgments.
  jsonl <- jsonlSink stdout o.model o.withRow
  (sink, startJ) <- case o.judgmentsDb of
    Nothing -> pure (jsonl, \_ -> pure ())
    Just jdb -> do
      ref <- newSinkRef
      let cfgJ j =
            S.builder o.host o.port jdb
              & S.withReconnect (S.Reconnect 500 30000 Nothing)
              & subscribeTable
                app.condition
                "SELECT * FROM condition"
                ( \ins dels -> do
                    let mine = filter (\(Condition _ t _ _ _) -> t == o.table)
                        insIds = [i | Condition i _ _ _ _ <- mine ins]
                    forM_ (mine dels) $ \(Condition i _ _ _ _) -> unless (i `elem` insIds) (removeCondition j i)
                    forM_ (mine ins) $ \(Condition i _ t k os) ->
                      either (logE . (("condition " <> i <> ": ") <>)) (upsertCondition j) (toQuestion i k t (T.lines os))
                    release j "conditions"
                )
              & subscribeTable
                app.judgment
                "SELECT * FROM judgment"
                ( \ins _ -> do
                    seedJudged j [(c, r, h) | Judgment _ c r h _ _ _ _ _ <- ins]
                    release j "judgments"
                )
              & S.onError (logE . tshow)
      pure (refSink ref, \j -> S.start (cfgJ j) >>= either die' (\c -> setSinkRef ref (spacetimeSink c o.table o.model logE)))

  judge <- newJudge jcfg jc sink onJ gates
  mapM_ (upsertCondition judge) cliQs
  startJ judge

  -- The source table.
  let srcCfg =
        S.builder o.host o.port o.db
          & S.withReconnect (S.Reconnect 500 30000 Nothing)
          & S.subscribeQuery
            o.table
            ("SELECT * FROM " <> o.table)
            ( \case
                S.TypedInitial bss -> do
                  its <- decodeAll bss
                  replaceRows judge [it | (_, Just it) <- its]
                  release judge "source"
                S.TypedChange ins dels -> do
                  is <- decodeAll ins
                  ds <- decodeAll dels
                  changeRows judge [it | (_, Just it) <- is] ([k | (k, _) <- ds] <> [k | (k, Nothing) <- is])
            )
          & S.onError (logE . tshow)
  void (startWorkers judge)
  src <- S.start srcCfg >>= either die' pure

  if o.once
    then do
      threadDelay 200000
      awaitIdle judge
      threadDelay 500000 -- let the last reducer calls flush
      readStats judge >>= hPutStrLn stderr . ("jev-judge: done " <>) . show
      S.stop src
    else forever (threadDelay maxBound)

tshow :: (Show a) => a -> Text
tshow = T.pack . show

-- The spacetime sink exists only once its client has connected, but the judge
-- needs a sink up front: route through a cell that blocks until it is filled.
newSinkRef :: IO (MVar Sink)
newSinkRef = newEmptyMVar

setSinkRef :: MVar Sink -> Sink -> IO ()
setSinkRef = putMVar

refSink :: MVar Sink -> Sink
refSink ref =
  Sink
    { writeJudgments = \q ps -> readMVar ref >>= \s -> s.writeJudgments q ps
    , forgetRow = \k -> readMVar ref >>= \s -> s.forgetRow k
    }
