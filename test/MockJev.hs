{- | A deterministic stand-in for a Jev-compatible endpoint (TypeSafe or
Intern-Decision), in the spirit of pg-jev's test/mock_api.py.

noul: P(yes) = 0.9 if the row's JSON contains the condition text, else 0.1.
choice: the first option that appears in the row's JSON, else the last option.
score: level 1. Every request's batch size is recorded.
-}
module MockJev (withMockJev, withMockJevCap) where

import Control.Concurrent.MVar
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Network.HTTP.Types (status200, status400, status422)
import Network.Wai (responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp qualified as Warp

-- | Run an action with the mock's base URL and a reader for recorded batch sizes.
withMockJev :: (Text -> IO [Int] -> IO a) -> IO a
withMockJev = withMockJevCap maxBound

{- | Like 'withMockJev', but refuse (422) requests with more than @cap@
questions, as Intern-Decision does above 16.
-}
withMockJevCap :: Int -> (Text -> IO [Int] -> IO a) -> IO a
withMockJevCap cap act = do
  seen <- newMVar []
  let app req respond = do
        body <- strictRequestBody req
        case decode body >>= answer of
          Nothing -> respond (responseLBS status400 [] "bad request")
          Just (n, _) | n > cap -> respond (responseLBS status422 [] "Supply 1-16 questions.")
          Just (n, out) -> do
            modifyMVar_ seen (pure . (n :))
            respond (responseLBS status200 [("Content-Type", "application/json")] (encode out))
  Warp.testWithApplication (pure app) $ \port ->
    act ("http://127.0.0.1:" <> T.pack (show port)) (reverse <$> readMVar seen)

answer :: Value -> Maybe (Int, Value)
answer = parseMaybe $ withObject "req" $ \o -> do
  st <- o .: "state"
  qs <- o .: "questions" :: Parser (KM.KeyMap Value)
  rows <- withObject "state" (.: "rows") st :: Parser (V.Vector Value)
  cond <- withObject "state" (\s -> s .:? "condition" .!= "") st :: Parser Text
  let rowText i = maybe "" (TE.decodeUtf8 . BL.toStrict . encode) (rows V.!? i)
      idx q = case T.breakOn "rows[" q of
        (_, rest) | not (T.null rest) -> read (T.unpack (T.takeWhile (/= ']') (T.drop 5 rest)))
        _ -> 0
  answers <-
    traverse
      ( \q ->
          withObject
            "q"
            ( \qo -> do
                ty <- qo .: "type" :: Parser Text
                instr <- qo .: "instructions" :: Parser Text
                let r = T.toLower (rowText (idx instr))
                case ty of
                  "noul" ->
                    let p = if T.toLower cond `T.isInfixOf` r then 0.9 else 0.1 :: Double
                     in pure (object ["type" .= ty, "noul" .= p, "confidence" .= max p (1 - p)])
                  "choice" -> do
                    crit <- qo .: "criteria" :: Parser (KM.KeyMap Value)
                    let opts = map Key.toText (KM.keys crit)
                        pick = fromMaybe (last opts) (lookupFirst (\x -> T.toLower x `T.isInfixOf` r) opts)
                        probs = KM.fromList [(Key.fromText x, Number (if x == pick then 0.8 else 0.2 / fromIntegral (length opts - 1))) | x <- opts]
                    pure (object ["type" .= ty, "choice" .= pick, "probabilities" .= probs, "confidence" .= (0.8 :: Double)])
                  _ ->
                    pure (object ["type" .= ty, "score" .= (1 :: Double), "legend" .= object [], "probabilities" .= object [], "confidence" .= (0.7 :: Double)])
            )
            q
      )
      qs
  pure (KM.size qs, object ["answers" .= answers, "usage" .= object ["input_tokens" .= (10 * KM.size qs), "output_tokens" .= (0 :: Int)]])
 where
  lookupFirst p = foldr (\x acc -> if p x then Just x else acc) Nothing
