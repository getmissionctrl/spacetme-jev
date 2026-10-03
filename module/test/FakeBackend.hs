module FakeBackend (newFake, backendFor, rowsOf) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Word (Word32)
import SpacetimeDB.Server.Internal (Backend (..), TableId (..))

-- The jev module has two tables; assign them stable ids.
tableIds :: M.Map Text Word32
tableIds = M.fromList [("condition", 1), ("judgment", 2)]

newFake :: IO (IORef (M.Map Word32 [ByteString]))
newFake = newIORef M.empty

backendFor :: IORef (M.Map Word32 [ByteString]) -> Backend
backendFor ref =
  Backend
    { tableId = \n -> pure (maybe (Left ("unknown table: " <> n)) (Right . TableId) (M.lookup n tableIds))
    , insert = \(TableId t) row -> modifyIORef' ref (M.insertWith (\new old -> old <> new) t [row]) >> pure (Right ())
    , scan = \(TableId t) -> Right . BS.concat . M.findWithDefault [] t <$> readIORef ref
    , delete = \(TableId t) row -> modifyIORef' ref (M.adjust (filter (/= row)) t) >> pure (Right ())
    , log = \_ -> pure ()
    }

rowsOf :: IORef (M.Map Word32 [ByteString]) -> Text -> IO [ByteString]
rowsOf ref n = maybe (pure []) (\t -> M.findWithDefault [] t <$> readIORef ref) (M.lookup n tableIds)
