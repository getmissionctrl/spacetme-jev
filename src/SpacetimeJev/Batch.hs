{- | One condition, many rows, one request. This is pg-jev's batching expressed
with hs-jev's free applicative: a batch of @n@ rows is
@traverse askRow [0 .. n-1]@, which hs-jev sends as a single call with
positional question keys and decodes back into a typed list.

Rows go into one shared state, @{"condition": ..., "rows": [...]}@, and each
question points at @rows[i]@. pg-jev measured position-lookup accuracy dropping
past ~20 rows per request on TypeSafe Jev; a small local model may want fewer,
so both the row count and a character budget cap a batch.
-}
module SpacetimeJev.Batch
  ( Kind (..)
  , Question (..)
  , Answer (..)
  , kindFromText
  , kindText
  , kindOptions
  , batchRequest
  , chunkBy
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Jev (Ask, ChoiceAnswer (..), ScoreAnswer (..), State, askChoice, askNoul, askScore, descNone, descPlain, plain, state)

data Kind
  = Noul
  | -- | options, each @name@ or @name: description@ (descriptions help a lot)
    Choice [Text]
  | -- | ordered levels, lowest first
    Score [Text]
  deriving (Eq, Show)

-- | A condition to keep answered.
data Question = Question
  { qid :: Text
  , text :: Text
  , kind :: Kind
  }
  deriving (Eq, Show)

-- | One row's answer, flattened to what the judgment table stores.
data Answer = Answer
  { value :: Double
  -- ^ noul: P(yes); score: probability-weighted level index; choice: P(chosen)
  , label :: Maybe Text
  -- ^ choice: the chosen option; score: the nearest level's name
  , confidence :: Double
  }
  deriving (Eq, Show)

kindFromText :: Text -> [Text] -> Either Text Kind
kindFromText k opts = case k of
  "noul" -> Right Noul
  "choice" | length opts >= 2 -> Right (Choice opts)
  "score" | length opts >= 2 && length opts <= 10 -> Right (Score opts)
  _ -> Left ("bad condition kind " <> k <> " with " <> T.pack (show (length opts)) <> " options")

kindText :: Kind -> Text
kindText = \case
  Noul -> "noul"
  Choice _ -> "choice"
  Score _ -> "score"

kindOptions :: Kind -> [Text]
kindOptions = \case
  Noul -> []
  Choice os -> os
  Score ls -> ls

-- | The state and the typed question batch for one condition over some rows.
batchRequest :: Question -> [Value] -> (State, Ask [Answer])
batchRequest q rows = (st, traverse askRow [0 .. length rows - 1])
 where
  st = case q.kind of
    Noul -> state (object ["condition" .= q.text, "rows" .= rows])
    _ -> state (object ["rows" .= rows])
  ref i = "`rows[" <> T.pack (show i) <> "]`"
  askRow :: Int -> Ask Answer
  askRow i = case q.kind of
    Noul ->
      (\p -> Answer p Nothing (max p (1 - p)))
        <$> askNoul (plain ("Does the record " <> ref i <> " satisfy the condition stated in `condition`?"))
    Choice os ->
      (\a -> Answer (Map.findWithDefault 0 a.choice a.probabilities) (Just a.choice) a.confidence)
        <$> askChoice (plain ("For the record " <> ref i <> ": " <> q.text)) (map option os)
    Score ls ->
      (\a -> Answer a.score (nearest ls a.score) a.confidence)
        <$> askScore (plain ("Rate the record " <> ref i <> ": " <> q.text)) (map plain ls)
  option o = case T.breakOn ": " o of
    (n, d) | not (T.null d) -> (T.strip n, descPlain (T.strip (T.drop 2 d)))
    _ -> (T.strip o, descNone)
  nearest ls s = case drop (round s) ls of
    (l : _) -> Just l
    [] -> Nothing

{- | Split items into batches of at most @maxN@ items and at most @maxSize@
total size (by the given measure). An item larger than the budget still gets
a batch of its own, so nothing is ever dropped.
-}
chunkBy :: Int -> Int -> (a -> Int) -> [a] -> [[a]]
chunkBy maxN maxSize size = finish . foldl' step ([], [], 0, 0)
 where
  step (done, cur, n, sz) x
    | n > 0 && (n >= maxN || sz + size x > maxSize) = (reverse cur : done, [x], 1, size x)
    | otherwise = (done, x : cur, n + 1, sz + size x)
  finish (done, cur, _, _) = reverse (if null cur then done else reverse cur : done)
