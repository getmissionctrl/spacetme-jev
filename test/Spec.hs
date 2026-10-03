module Main (main) where

import Control.Monad (void)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.WideWord (Word256)
import Data.Word (Word64, Word8)
import Jev qualified
import MockJev (withMockJev, withMockJevCap)
import SpacetimeDB.BSATN.Decoder (runExact)
import SpacetimeDB.BSATN.Encoder
import SpacetimeDB.Codegen.Schema (parseModule)
import SpacetimeJev.Batch
import SpacetimeJev.Dynamic
import SpacetimeJev.Judge
import System.Timeout (timeout)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "chunkBy" $ do
    it "caps batches by count" $
      chunkBy 20 maxBound (const 1) [1 .. 45 :: Int] `shouldBe` [[1 .. 20], [21 .. 40], [41 .. 45]]
    it "caps batches by size, never dropping an oversized item" $
      chunkBy 20 10 id [4, 4, 4, 30, 2 :: Int] `shouldBe` [[4, 4], [4], [30], [2]]

  describe "batchRequest" $ do
    it "puts n rows in one state with one positional question each" $ do
      let (_, ask) = batchRequest (Question "c" "is angry" Noul) [String "a", String "b", String "c"]
          qs = Jev.encodeQuestions ask
      KM.size qs `shouldBe` 3
      show (KM.lookup "q2" qs) `shouldContain` "rows[2]"

  describe "Dynamic.rowCodec" $ do
    it "decodes a row to JSON from the schema alone" $ do
      raw <- BL.readFile "test/fixtures/lead.schema.json"
      m <- either fail pure (parseModule raw)
      codec <- either (fail . T.unpack) pure (rowCodec m "lead")
      codec.columns `shouldBe` ["id", "name", "research", "owner", "at", "stage", "tags", "score"]
      runExact codec.decodeRow (leadRow 7 "Acme" (Just "a founder-led consultancy in Dubai") 2 ["uk"])
        `shouldBe` Right
          ( object
              [ "id" .= (7 :: Int)
              , "name" .= ("Acme" :: Text)
              , "research" .= ("a founder-led consultancy in Dubai" :: Text)
              , "owner" .= ("5" :: Text)
              , "at" .= (1000 :: Int)
              , "stage" .= object ["Note" .= ("call back" :: Text)]
              , "tags" .= (["uk"] :: [Text])
              , "score" .= (0.5 :: Double)
              ]
          )
      fmap (KM.lookup "research") (asObj (runExact codec.decodeRow (leadRow 8 "Blank" Nothing 0 [])))
        `shouldBe` Right (Just Null)
      fmap (KM.lookup "stage") (asObj (runExact codec.decodeRow (leadRow 8 "Blank" Nothing 1 [])))
        `shouldBe` Right (Just (String "Won"))

  describe "Judge" $ do
    it "batches an initial load, caches by content, and re-judges only changes" $
      withMockJev $ \url batches -> do
        (j, out, forgotten) <- judgeAgainst url defaultJudgeConfig {batchSize = 20, workers = 2}
        upsertCondition j (Question "dubai" "dubai" Noul)
        replaceRows j [row i (if even i then "in Dubai" else "in Leeds") | i <- [1 .. 45 :: Int]]
        release j "source"
        settle j
        sum <$> batches `shouldReturn` 45
        (maximum <$> batches) `shouldReturn` 20
        m <- readIORef out
        Map.size m `shouldBe` 45
        fmap (.value) (Map.lookup ("dubai", "2") m) `shouldBe` Just 0.9
        fmap (.value) (Map.lookup ("dubai", "3") m) `shouldBe` Just 0.1

        -- Same content again (a reconnect): nothing is sent.
        n0 <- length <$> batches
        replaceRows j [row i (if even i then "in Dubai" else "in Leeds") | i <- [1 .. 45 :: Int]]
        settle j
        length <$> batches `shouldReturn` n0
        (.cacheHits) <$> readStats j `shouldReturn` 45

        -- One row changes, one disappears.
        changeRows j [row 3 "moved to Dubai"] ["44"]
        settle j
        drop n0 <$> batches `shouldReturn` [1]
        fmap (.value) . Map.lookup ("dubai", "3") <$> readIORef out `shouldReturn` Just 0.9
        readIORef forgotten `shouldReturn` ["44"]

    it "judges every current row when a condition arrives, and again when it changes" $
      withMockJev $ \url batches -> do
        (j, out, _) <- judgeAgainst url defaultJudgeConfig {batchSize = 4, workers = 1}
        replaceRows j [row i "billing problem" | i <- [1 .. 6 :: Int]]
        release j "source"
        upsertCondition j (Question "team" "which team?" (Choice ["billing", "technical"]))
        settle j
        batches `shouldReturn` [4, 2]
        fmap (.label) . Map.lookup ("team", "1") <$> readIORef out `shouldReturn` Just (Just "billing")
        upsertCondition j (Question "team" "which team?" (Choice ["sales", "problem"]))
        settle j
        length <$> batches `shouldReturn` 4
        fmap (.label) . Map.lookup ("team", "1") <$> readIORef out `shouldReturn` Just (Just "problem")

    it "splits a batch the server refuses until it fits" $
      withMockJevCap 16 $ \url batches -> do
        (j, out, _) <- judgeAgainst url defaultJudgeConfig {batchSize = 20, workers = 1}
        upsertCondition j (Question "c" "x" Noul)
        replaceRows j [row i "x" | i <- [1 .. 40 :: Int]]
        release j "source"
        settle j
        batches `shouldReturn` [10, 10, 10, 10]
        Map.size <$> readIORef out `shouldReturn` 40

    it "sends choice descriptions and returns option names" $
      withMockJev $ \url _ -> do
        (j, out, _) <- judgeAgainst url defaultJudgeConfig
        replaceRows j [row 1 "a win for sure"]
        release j "source"
        upsertCondition j (Question "fit" "fits?" (Choice ["Win: worth pursuing", "Lose: pass"]))
        settle j
        fmap (.label) . Map.lookup ("fit", "1") <$> readIORef out `shouldReturn` Just (Just "Win")

    it "respects the character budget and the spend cap" $
      withMockJev $ \url batches -> do
        events <- newIORef []
        (j, _, _) <- judgeAgainstWith url defaultJudgeConfig {batchSize = 20, maxBatchChars = 200, workers = 1, maxRows = Just 5} (\e -> modifyIORef events (e :))
        upsertCondition j (Question "c" "x" Noul)
        replaceRows j [row i (T.replicate 60 "y") | i <- [1 .. 10 :: Int]]
        release j "source"
        settle j
        bs <- batches
        sum bs `shouldSatisfy` (<= 5)
        all (<= 2) bs `shouldBe` True
        es <- readIORef events
        length [() | SpendCapReached _ <- es] `shouldBe` 1

-- Helpers ---------------------------------------------------------------------

row :: Int -> Text -> Item
row i t = mkItem (T.pack (show i)) (object ["note" .= t])

judgeAgainst :: Text -> JudgeConfig -> IO (Judge, IORef (Map.Map (Text, Text) Answer), IORef [Text])
judgeAgainst url cfg = judgeAgainstWith url cfg (const (pure ()))

judgeAgainstWith :: Text -> JudgeConfig -> (JudgeEvent -> IO ()) -> IO (Judge, IORef (Map.Map (Text, Text) Answer), IORef [Text])
judgeAgainstWith url cfg onEv = do
  jc <- Jev.newClientWith (Jev.defaultConfig "test") {Jev.baseUrl = url, Jev.endpoint = "/v1/jev"}
  out <- newIORef Map.empty
  forgotten <- newIORef []
  let sink =
        Sink
          { writeJudgments = \q ps -> modifyIORef out (\m -> foldr (\(i, a) -> Map.insert (q.qid, i.key) a) m ps)
          , forgetRow = \k -> modifyIORef forgotten (k :)
          }
  j <- newJudge cfg {lingerMicros = 5000} jc sink onEv ["source"]
  void (startWorkers j)
  pure (j, out, forgotten)

settle :: Judge -> IO ()
settle j = timeout 5000000 (awaitIdle j) >>= maybe (expectationFailure "judge did not go idle") pure

asObj :: Either e Value -> Either e (KM.KeyMap Value)
asObj = fmap (\case Object o -> o; _ -> KM.empty)

-- A lead row in BSATN, laid out as test/fixtures/lead.schema.json says.
leadRow :: Word64 -> Text -> Maybe Text -> Word8 -> [Text] -> ByteString
leadRow i n research stageTag tags =
  runEncoder id $
    concatE
      [ encodeU64 i
      , encodeString n
      , encodeOptional research encodeString
      , encodeU256 (5 :: Word256)
      , encodeI64 1000
      , encodeSum stageTag (if stageTag == 2 then encodeString "call back" else mempty)
      , encodeList tags encodeString
      , encodeF64 0.5
      ]
