{-# LANGUAGE ForeignFunctionInterface #-}

{- | WASI-free wasm reactor entry point for the jev module.
Re-exports the SpacetimeDB C ABI ('__describe_module__' / '__call_reducer__')
over 'JevSchema.jevModule'. Mirrors @server/example/PersonModule.hs@.
-}
module JevModule where

import Data.Int (Int16)
import Data.Word (Word32, Word64)
import JevSchema (jevModule)
import SpacetimeDB.Server.ABI (runCallReducer, runDescribe)

foreign export ccall hs_describe :: Word32 -> IO ()
hs_describe :: Word32 -> IO ()
hs_describe = runDescribe jevModule

foreign export ccall
  hs_call_reducer
    :: Word32
    -> Word64
    -> Word64
    -> Word64
    -> Word64
    -> Word64
    -> Word64
    -> Word64
    -> Word32
    -> Word32
    -> IO Int16
hs_call_reducer
  :: Word32
  -> Word64
  -> Word64
  -> Word64
  -> Word64
  -> Word64
  -> Word64
  -> Word64
  -> Word32
  -> Word32
  -> IO Int16
hs_call_reducer = runCallReducer jevModule
