{- | The sidecar engine. It holds the current rows of one source table and the
conditions asked about it, and keeps a judgment current for every
(condition, row) pair:

  * a row is (re)judged only when the content it sends changes (content hash),
    so restarts and reconnects cost nothing once judgments are seeded;
  * work is queued per (condition, row) and workers pull same-condition runs
    off the queue into batches, so an initial load of N rows costs about
    N / batch-size requests while a live insert is judged on its own, at once;
  * a result is written only if the row still has the content that was judged.

Concurrency is plain STM + 'forkIO'. Subscription callbacks only touch STM, so
they never block hs-spacetime's receive thread.
-}
module SpacetimeJev.Judge
  ( -- * Rows
    Item (..)
  , mkItem

    -- * Configuration and events
  , JudgeConfig (..)
  , defaultJudgeConfig
  , JudgeEvent (..)
  , Stats (..)
  , Sink (..)

    -- * Running
  , Judge
  , newJudge
  , startWorkers
  , release
  , replaceRows
  , changeRows
  , upsertCondition
  , removeCondition
  , seedJudged
  , awaitIdle
  , readStats
  ) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, forever, unless, when)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (Value, encode)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Clock (getMonotonicTimeNSec)
import Jev (Usage (..), callJev)
import Jev qualified
import SpacetimeJev.Batch

-- | A source row as the judge sees it: its key and the JSON that gets judged.
data Item = Item
  { key :: Text
  , hash :: Text
  , payload :: Value
  , size :: Int
  }
  deriving (Eq, Show)

mkItem :: Text -> Value -> Item
mkItem k v =
  let bs = BL.toStrict (encode v)
   in Item k (TE.decodeUtf8 (B16.encode (SHA256.hash bs))) v (fromIntegral (BL.length (BL.fromStrict bs)))

data JudgeConfig = JudgeConfig
  { batchSize :: Int
  -- ^ rows per request (pg-jev: 20 for TypeSafe Jev; start lower on a small local model)
  , maxBatchChars :: Int
  -- ^ JSON characters of row data per request (keeps a batch inside the model's context)
  , workers :: Int
  -- ^ requests in flight
  , lingerMicros :: Int
  -- ^ after taking work, wait this long for more to batch with it
  , maxRows :: Maybe Int
  -- ^ spend guard: stop sending after this many rows (per process)
  }
  deriving (Show)

defaultJudgeConfig :: JudgeConfig
defaultJudgeConfig = JudgeConfig {batchSize = 20, maxBatchChars = 24000, workers = 4, lingerMicros = 20000, maxRows = Nothing}

data JudgeEvent
  = BatchDone {condition :: Text, rows :: Int, inputTokens :: Int, millis :: Int}
  | BatchFailed {condition :: Text, rows :: Int, reason :: Text}
  | SpendCapReached Int
  | SinkFailed Text
  deriving (Show)

data Stats = Stats
  { requests :: !Int
  , rowsJudged :: !Int
  , inputTokens :: !Int
  , cacheHits :: !Int
  , failures :: !Int
  , stale :: !Int
  -- ^ answers dropped because the row changed while it was being judged
  }
  deriving (Eq, Show)

-- | Where answers go.
data Sink = Sink
  { writeJudgments :: Question -> [(Item, Answer)] -> IO ()
  , forgetRow :: Text -> IO ()
  }

data Judge = Judge
  { cfg :: JudgeConfig
  , client :: Jev.Client
  , sink :: Sink
  , onJudge :: JudgeEvent -> IO ()
  , conds :: TVar (Map Text Question)
  , items :: TVar (Map Text Item)
  , done :: TVar (Map (Text, Text) Text)
  -- ^ (condition, row key) -> hash of the content last judged
  , pending :: TVar (Set (Text, Text))
  , queue :: TQueue (Text, Text)
  , inflight :: TVar Int
  , gate :: TVar (Set Text)
  -- ^ initial loads still outstanding; workers wait until it is empty
  , capHit :: TVar Bool
  -- ^ the spend cap has been reported
  , stats :: TVar Stats
  }

newJudge :: JudgeConfig -> Jev.Client -> Sink -> (JudgeEvent -> IO ()) -> [Text] -> IO Judge
newJudge cfg client sink onJudge waitFor =
  Judge cfg client sink onJudge
    <$> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO Set.empty
    <*> newTQueueIO
    <*> newTVarIO 0
    <*> newTVarIO (Set.fromList waitFor)
    <*> newTVarIO False
    <*> newTVarIO (Stats 0 0 0 0 0 0)

-- | Mark an initial load as delivered (idempotent, so reconnects are harmless).
release :: Judge -> Text -> IO ()
release j name = atomically (modifyTVar' j.gate (Set.delete name))

readStats :: Judge -> IO Stats
readStats j = readTVarIO j.stats

-- Queueing --------------------------------------------------------------------

enqueue :: Judge -> Text -> Item -> STM ()
enqueue j cid it = do
  d <- readTVar j.done
  let pk = (cid, it.key)
  if Map.lookup pk d == Just it.hash
    then modifyTVar' j.stats (\s -> s {cacheHits = s.cacheHits + 1})
    else do
      p <- readTVar j.pending
      unless (Set.member pk p) $ do
        writeTVar j.pending (Set.insert pk p)
        writeTQueue j.queue pk

-- | Apply a subscription change. Keys present in both lists are updates.
changeRows :: Judge -> [Item] -> [Text] -> IO ()
changeRows j ins dels = do
  gone <- atomically $ do
    let insKeys = Set.fromList (map (.key) ins)
        gone = filter (`Set.notMember` insKeys) dels
    modifyTVar' j.items (\m -> foldr Map.delete m gone)
    modifyTVar' j.done (Map.filterWithKey (\(_, k) _ -> k `notElem` gone))
    modifyTVar' j.items (\m -> foldr (\it -> Map.insert it.key it) m ins)
    cs <- Map.keys <$> readTVar j.conds
    forM_ ins (\it -> forM_ cs (\c -> enqueue j c it))
    pure gone
  forM_ gone (safely j . j.sink.forgetRow)

-- | Replace the whole row set (an initial load, including after a reconnect).
replaceRows :: Judge -> [Item] -> IO ()
replaceRows j new = do
  old <- Map.keys <$> readTVarIO j.items
  let newKeys = Set.fromList (map (.key) new)
  changeRows j new (filter (`Set.notMember` newKeys) old)

-- | Add a condition, or replace it; a changed condition invalidates its judgments.
upsertCondition :: Judge -> Question -> IO ()
upsertCondition j q = atomically $ do
  prev <- Map.lookup q.qid <$> readTVar j.conds
  when (prev /= Just q) $ do
    when (prev /= Nothing) $
      modifyTVar' j.done (Map.filterWithKey (\(c, _) _ -> c /= q.qid))
    modifyTVar' j.conds (Map.insert q.qid q)
    readTVar j.items >>= mapM_ (enqueue j q.qid) . Map.elems

removeCondition :: Judge -> Text -> IO ()
removeCondition j cid = atomically $ do
  modifyTVar' j.conds (Map.delete cid)
  modifyTVar' j.done (Map.filterWithKey (\(c, _) _ -> c /= cid))

-- | Record judgments that already exist (e.g. read back from the judgment table).
seedJudged :: Judge -> [(Text, Text, Text)] -> IO ()
seedJudged j xs =
  atomically $
    modifyTVar' j.done (\m -> foldr (\(c, k, h) -> Map.insert (c, k) h) m xs)

-- | Block until the gate is open, the queue is drained and nothing is in flight.
awaitIdle :: Judge -> IO ()
awaitIdle j = atomically $ do
  g <- readTVar j.gate
  e <- isEmptyTQueue j.queue
  n <- readTVar j.inflight
  unless (Set.null g && e && n == 0) retry

-- Workers ---------------------------------------------------------------------

startWorkers :: Judge -> IO [ThreadId]
startWorkers j = mapM (const (forkIO (forever (worker j)))) [1 .. max 1 j.cfg.workers]

data Work = Work Question [Item] | Skip | Capped

worker :: Judge -> IO ()
worker j = do
  first <- atomically $ do
    g <- readTVar j.gate
    unless (Set.null g) retry
    pk <- readTQueue j.queue
    modifyTVar' j.inflight (+ 1)
    pure pk
  when (j.cfg.lingerMicros > 0) (threadDelay j.cfg.lingerMicros)
  work <- atomically (collect j first)
  case work of
    Skip -> pure ()
    Capped -> do
      first' <- atomically (not <$> swapTVar j.capHit True)
      when first' (readStats j >>= j.onJudge . SpendCapReached . (.rowsJudged))
    Work q its -> judgeBatch j q its
  atomically (modifyTVar' j.inflight (subtract 1))

-- Take @first@ plus as many queued same-condition rows as fit one batch.
collect :: Judge -> (Text, Text) -> STM Work
collect j (cid, k0) = do
  mq <- Map.lookup cid <$> readTVar j.conds
  rows <- readTVar j.items
  d <- readTVar j.done
  let current (c, k) = case Map.lookup k rows of
        Just it | Map.lookup (c, k) d /= Just it.hash -> Just it
        _ -> Nothing
      go acc n sz = do
        if n >= j.cfg.batchSize
          then pure acc
          else
            tryReadTQueue j.queue >>= \case
              Nothing -> pure acc
              Just pk@(c, _)
                | c /= cid -> unGetTQueue j.queue pk >> pure acc
                | otherwise -> case current pk of
                    Nothing -> modifyTVar' j.pending (Set.delete pk) >> go acc n sz
                    Just it
                      | sz + it.size > j.cfg.maxBatchChars -> unGetTQueue j.queue pk >> pure acc
                      | otherwise -> modifyTVar' j.pending (Set.delete pk) >> go (it : acc) (n + 1) (sz + it.size)
  modifyTVar' j.pending (Set.delete (cid, k0))
  case (mq, current (cid, k0)) of
    (Just q, Just it0) -> do
      rest <- go [] 1 it0.size
      let its = it0 : reverse rest
      s <- readTVar j.stats
      case j.cfg.maxRows of
        Just cap | s.rowsJudged + length its > cap -> do
          modifyTVar' j.pending (\p -> foldr (\it -> Set.delete (cid, it.key)) p its)
          pure Capped
        _ -> pure (Work q its)
    _ -> pure Skip

judgeBatch :: Judge -> Question -> [Item] -> IO ()
judgeBatch j q its = do
  let (st, ask) = batchRequest q (map (.payload) its)
  t0 <- getMonotonicTimeNSec
  r <- try (callJev j.client st ask)
  t1 <- getMonotonicTimeNSec
  let ms = fromIntegral ((t1 - t0) `div` 1000000)
  case r of
    Left (e :: SomeException) -> failed (T.pack (displayException e))
    -- The server refused the batch as a whole (Intern-Decision: over its token
    -- window, or more than 16 questions). Halve it and try each half.
    Right (Left (Jev.ApiError code _))
      | code `elem` [400, 413, 422]
      , length its > 1 -> do
          let (a, b) = splitAt (length its `div` 2) its
          judgeBatch j q a >> judgeBatch j q b
    Right (Left err) -> failed (T.pack (show err))
    Right (Right (answers, usage)) -> do
      fresh <- atomically $ do
        rows <- readTVar j.items
        cur <- Map.lookup q.qid <$> readTVar j.conds
        let pairs = zip its answers
            keep = [p | cur == Just q, p@(it, _) <- pairs, fmap (.hash) (Map.lookup it.key rows) == Just it.hash]
        modifyTVar' j.done (\m -> foldr (\(it, _) -> Map.insert (q.qid, it.key) it.hash) m keep)
        modifyTVar' j.stats $ \s ->
          s
            { requests = s.requests + 1
            , rowsJudged = s.rowsJudged + length its
            , inputTokens = s.inputTokens + usage.inputTokens
            , stale = s.stale + (length pairs - length keep)
            }
        pure keep
      j.onJudge (BatchDone q.qid (length its) usage.inputTokens ms)
      unless (null fresh) (safely j (j.sink.writeJudgments q fresh))
 where
  failed why = do
    atomically (modifyTVar' j.stats (\s -> s {failures = s.failures + 1}))
    j.onJudge (BatchFailed q.qid (length its) why)

safely :: Judge -> IO () -> IO ()
safely j act =
  try act >>= \case
    Left (e :: SomeException) -> j.onJudge (SinkFailed (T.pack (displayException e)))
    Right () -> pure ()
