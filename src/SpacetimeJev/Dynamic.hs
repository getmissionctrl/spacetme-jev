{- | Schema-driven BSATN → JSON. Given the module schema from
@spacetime describe --json <db>@, decode any table's rows into Aeson values
with their real column names, so the judge needs no per-table Haskell type,
no codegen and no 'ToJSON' instance. This plays the part of pg-jev's
@to_json(row)@.

Rendering: named products become objects (unnamed ones arrays); options become
the value or @null@; other sums become @{"variant": payload}@ (a bare string for
unit variants); SpacetimeDB's special products (@Identity@, @ConnectionId@,
@Timestamp@, @TimeDuration@) collapse to their single scalar.
-}
module SpacetimeJev.Dynamic
  ( RowCodec (..)
  , rowCodec
  , renderKey
  ) where

import Control.Monad (replicateM)
import Data.Aeson (Value (..), object)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as B16
import Data.Scientific qualified as Sci
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import SpacetimeDB.BSATN.Decoder
import SpacetimeDB.Codegen.Schema (Field (..), Module (..), TableDef (..), Typ (..), resolve)

-- | How to read one table's rows.
data RowCodec = RowCodec
  { columns :: [Text]
  , decodeRow :: Decoder Value
  -- ^ always an 'Object' keyed by column name
  }

-- | Build the decoder for a table by name.
rowCodec :: Module -> Text -> Either Text RowCodec
rowCodec m tbl = do
  td <-
    maybe (Left ("no table named " <> tbl <> " (tables: " <> T.intercalate ", " (map (.tblName) m.modTables) <> ")")) Right $
      lookupTable
  ty <- resolve m (TRef td.tblProductRef)
  case ty of
    TProduct fs -> do
      let names = zipWith (\i f -> maybe ("col" <> tshow i) id f.fieldName) [0 :: Int ..] fs
      decs <- traverse (typDecoder m . (.fieldTy)) fs
      pure
        RowCodec
          { columns = names
          , decodeRow = object . zipWith (\n v -> (Key.fromText n, v)) names <$> sequence decs
          }
    _ -> Left ("table " <> tbl <> " is not a product type")
 where
  lookupTable = case filter (\t -> t.tblName == tbl) m.modTables of
    (t : _) -> Just t
    [] -> Nothing

-- Resolve the type tree up front so decoding never fails on a dangling ref.
-- Recursive types would loop here; SpacetimeDB table rows cannot be recursive.
typDecoder :: Module -> Typ -> Either Text (Decoder Value)
typDecoder m = go
 where
  go = \case
    TRef i -> resolve m (TRef i) >>= go
    TPrim p -> prim p
    TArray (TPrim "U8") -> pure (String . hex <$> bytes)
    TArray t -> do
      d <- go t
      pure $ do
        n <- u32
        Array . V.fromList <$> replicateM (fromIntegral n) d
    TProduct [Field (Just n) t]
      | n `elem` specialScalars -> go t
    TProduct fs
      | all (\f -> f.fieldName /= Nothing) fs -> do
          ds <- traverse (\f -> (,) (maybe "" id f.fieldName) <$> go f.fieldTy) fs
          pure (object <$> traverse (\(n, d) -> (,) (Key.fromText n) <$> d) ds)
      | otherwise -> do
          ds <- traverse (go . (.fieldTy)) fs
          pure (Array . V.fromList <$> sequence ds)
    TSum [Field (Just "some") t, Field (Just "none") _] -> do
      d <- go t
      pure (maybe Null id <$> optional d)
    TSum vs -> do
      ds <- traverse (\f -> (,,) (maybe "" id f.fieldName) (isUnit f.fieldTy) <$> go f.fieldTy) vs
      pure $ sumD $ \tag -> case drop (fromIntegral tag) ds of
        ((n, True, d) : _) -> Right (String n <$ d)
        ((n, False, d) : _) -> Right ((\v -> Object (KM.singleton (Key.fromText n) v)) <$> d)
        [] -> Left (UnknownVariant tag)

  isUnit = \case
    TProduct [] -> True
    TRef i -> either (const False) isUnit (resolve m (TRef i))
    _ -> False

  prim :: Text -> Either Text (Decoder Value)
  prim = \case
    "Bool" -> pure (Bool <$> bool)
    "String" -> pure (String <$> string)
    "U8" -> pure (num <$> u8)
    "U16" -> pure (num <$> u16)
    "U32" -> pure (num <$> u32)
    "U64" -> pure (num <$> u64)
    "I8" -> pure (num <$> i8)
    "I16" -> pure (num <$> i16)
    "I32" -> pure (num <$> i32)
    "I64" -> pure (num <$> i64)
    -- 128/256-bit values exceed JSON number precision in most readers: render as text.
    "U128" -> pure (String . tshow . toInteger <$> u128)
    "I128" -> pure (String . tshow . toInteger <$> i128)
    "U256" -> pure (String . tshow . toInteger <$> u256)
    "I256" -> pure (String . tshow . toInteger <$> i256)
    "F32" -> pure (Number . Sci.fromFloatDigits <$> f32)
    "F64" -> pure (Number . Sci.fromFloatDigits <$> f64)
    other -> Left ("unsupported primitive type " <> other)

  num :: (Integral a) => a -> Value
  num = Number . fromIntegral

specialScalars :: [Text]
specialScalars =
  [ "__identity__"
  , "__connection_id__"
  , "__timestamp_micros_since_unix_epoch__"
  , "__time_duration_micros__"
  , "__uuid__"
  ]

hex :: ByteString -> Text
hex = TE.decodeUtf8 . B16.encode

-- | A column value as a stable text key (for @row_key@).
renderKey :: Value -> Text
renderKey = \case
  String t -> t
  Number n -> either (tshow @Double) (tshow @Integer) (Sci.floatingOrInteger n)
  Bool b -> if b then "true" else "false"
  Null -> ""
  other -> tshow other

tshow :: (Show a) => a -> Text
tshow = T.pack . show
