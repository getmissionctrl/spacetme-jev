{- | Where judgments go.

  * 'jsonlSink' prints one JSON line per judgment: the quickest way to test a
    model against a live table without publishing anything.
  * 'spacetimeSink' writes them into the jev module's @judgment@ table via its
    reducers, where any client can subscribe to them.
-}
module SpacetimeJev.Sink
  ( jsonlSink
  , spacetimeSink
  , teeSink
  ) where

import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Monad (forM_)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as A
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text (Text)
import JevSchema (App (..), ForgetRowArgs (..), RecordJudgmentArgs (..), app)
import SpacetimeDB.BSATN.Decoder (string, success)
import SpacetimeDB.Client (Client, ReducerReply (..), reducerReply)
import SpacetimeDB.Client.Typed (callTyped)
import SpacetimeJev.Batch
import SpacetimeJev.Judge (Item (..), Sink (..))
import System.IO (Handle, hFlush)

-- | One JSON object per line. @withRow@ includes the judged payload.
jsonlSink :: Handle -> Text -> Bool -> IO Sink
jsonlSink h model withRow = do
  lock <- newMVar ()
  let line v = withMVar lock (\_ -> BL.hPutStrLn h (A.encode v) >> hFlush h)
  pure
    Sink
      { writeJudgments = \q pairs -> forM_ pairs $ \(it, a) ->
          line . object $
            [ "condition" .= q.qid
            , "kind" .= kindText q.kind
            , "row_key" .= it.key
            , "row_hash" .= it.hash
            , "value" .= a.value
            , "label" .= a.label
            , "confidence" .= a.confidence
            , "model" .= model
            ]
              <> ["row" .= it.payload | withRow]
      , forgetRow = \k -> line (object ["forget" .= k])
      }

{- | Write through the jev module's reducers on @client@ (connected to the
judgment database). @onFail@ hears about rejected calls.
-}
spacetimeSink :: Client -> Text -> Text -> (Text -> IO ()) -> Sink
spacetimeSink c sourceTable model onFail =
  Sink
    { writeJudgments = \q pairs -> forM_ pairs $ \(it, a) ->
        callTyped
          c
          app.recordJudgment
          RecordJudgmentArgs
            { condition = q.qid
            , rowKey = it.key
            , rowHash = it.hash
            , value = a.value
            , label = a.label
            , confidence = a.confidence
            , model = model
            }
          (check "record_judgment")
    , forgetRow = \k -> callTyped c app.forgetRow (ForgetRowArgs sourceTable k) (check "forget_row")
    }
 where
  check what reply = case reducerReply (success ()) string reply of
    Failed e -> onFail (what <> ": " <> e)
    ReducerCallFailed e -> onFail (what <> ": " <> e)
    _ -> pure ()

teeSink :: Sink -> Sink -> Sink
teeSink a b =
  Sink
    { writeJudgments = \q ps -> a.writeJudgments q ps >> b.writeJudgments q ps
    , forgetRow = \k -> a.forgetRow k >> b.forgetRow k
    }
