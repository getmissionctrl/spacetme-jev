{- | The judgment database: conditions people ask about, and the judgments the
sidecar writes back. Rows of the judged table never live here; a judgment
points at them by @row_key@ (the source table's primary key, rendered as text),
so the source database needs no changes.

Wire names (snake_case) come from the field names via 'deriveApp':
tables @condition@ / @judgment@; reducers @add_condition@, @remove_condition@,
@record_judgment@, @forget_row@.
-}
module JevSchema
  ( Condition (..)
  , Judgment (..)
  , AddConditionArgs (..)
  , RemoveConditionArgs (..)
  , RecordJudgmentArgs (..)
  , ForgetRowArgs (..)
  , App (..)
  , app
  , jevModule
  , judgmentKey
  , validKinds
  ) where

import Control.Monad (forM_, unless, when)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import SpacetimeDB.BSATN.Types (Timestamp)
import SpacetimeDB.Server (ModuleDef, ask, deleteRow, deriveApp, insertRow, scanRows, throwError)
import SpacetimeDB.Server.Derive (deriveModule)
import SpacetimeDB.Server.HKD (ColAttr (..), Column, View (..))
import SpacetimeDB.Server.Internal (ReducerContext (..))
import SpacetimeDB.Server.Reducer (Reducer)
import SpacetimeDB.Server.SpacetimeType (SpacetimeType (..))
import SpacetimeDB.Server.Table (Table)
import SpacetimeDB.Server.Types (ReducerM)

-- | A plain-language question to keep answered for every row of a source table.
data Condition f = Condition
  { id :: Column f Text '[ 'Pk]
  , sourceTable :: Column f Text '[]
  , text :: Column f Text '[]
  , kind :: Column f Text '[]
  -- ^ @noul@ (yes/no), @choice@ or @score@
  , options :: Column f Text '[]
  -- ^ newline-separated choice options or ordered score levels; empty for noul
  }
  deriving stock (Generic)

deriving anyclass instance SpacetimeType (Condition 'Value)
deriving stock instance Eq (Condition 'Value)
deriving stock instance Show (Condition 'Value)

{- | One answer for one (condition, row). @value@ is the yes-probability for
noul, the probability-weighted level for score, and the winning option's
probability for choice (whose option is @label@). @row_hash@ is the hash of
what was sent, so the judge only re-asks when the row's judged content changes.
-}
data Judgment f = Judgment
  { key :: Column f Text '[ 'Pk]
  , condition :: Column f Text '[]
  , rowKey :: Column f Text '[]
  , rowHash :: Column f Text '[]
  , value :: Column f Double '[]
  , label :: Column f (Maybe Text) '[]
  , confidence :: Column f Double '[]
  , model :: Column f Text '[]
  , at :: Column f Timestamp '[]
  }
  deriving stock (Generic)

deriving anyclass instance SpacetimeType (Judgment 'Value)
deriving stock instance Eq (Judgment 'Value)
deriving stock instance Show (Judgment 'Value)

data AddConditionArgs = AddConditionArgs
  { id :: Text
  , sourceTable :: Text
  , text :: Text
  , kind :: Text
  , options :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (SpacetimeType)

newtype RemoveConditionArgs = RemoveConditionArgs {id :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (SpacetimeType)

data RecordJudgmentArgs = RecordJudgmentArgs
  { condition :: Text
  , rowKey :: Text
  , rowHash :: Text
  , value :: Double
  , label :: Maybe Text
  , confidence :: Double
  , model :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (SpacetimeType)

data ForgetRowArgs = ForgetRowArgs {sourceTable :: Text, rowKey :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (SpacetimeType)

data App = App
  { condition :: Table Condition
  , judgment :: Table Judgment
  , addCondition :: Reducer AddConditionArgs
  , removeCondition :: Reducer RemoveConditionArgs
  , recordJudgment :: Reducer RecordJudgmentArgs
  , forgetRow :: Reducer ForgetRowArgs
  }
  deriving stock (Generic)

app :: App
app = deriveApp

data Handlers = Handlers
  { addCondition :: AddConditionArgs -> ReducerM ()
  , removeCondition :: RemoveConditionArgs -> ReducerM ()
  , recordJudgment :: RecordJudgmentArgs -> ReducerM ()
  , forgetRow :: ForgetRowArgs -> ReducerM ()
  }
  deriving stock (Generic)

validKinds :: [Text]
validKinds = ["noul", "choice", "score"]

-- | Primary key of a judgment: one row per (condition, source row).
judgmentKey :: Text -> Text -> Text
judgmentKey c r = c <> "\x1f" <> r

-- The ABI has no index lookups yet, so key lookups are scans. Fine for
-- thousands of judgments; needs btree index scans before it is fine for millions.
deleteWhere :: (Judgment 'Value -> Bool) -> ReducerM ()
deleteWhere p = scanRows app.judgment >>= \js -> forM_ (filter p js) (deleteRow app.judgment)

jevModule :: ModuleDef
jevModule =
  deriveModule
    app
    Handlers
      { addCondition = \a -> do
          when (T.null a.id) (throwError "condition id must not be empty")
          when (T.null a.text) (throwError "condition text must not be empty")
          unless (a.kind `elem` validKinds) (throwError ("kind must be one of " <> T.intercalate ", " validKinds))
          let opts = filter (not . T.null) (T.lines a.options)
          when (a.kind /= "noul" && length opts < 2) (throwError "choice/score need at least two options")
          when (a.kind == "score" && length opts > 10) (throwError "score takes at most ten levels")
          -- Re-adding an id replaces the condition and drops its stale judgments.
          existing <- filter (\(Condition i _ _ _ _) -> i == a.id) <$> scanRows app.condition
          forM_ existing (deleteRow app.condition)
          deleteWhere (\(Judgment _ c _ _ _ _ _ _ _) -> c == a.id)
          insertRow app.condition (Condition a.id a.sourceTable a.text a.kind (T.intercalate "\n" opts))
      , removeCondition = \(RemoveConditionArgs cid) -> do
          existing <- filter (\(Condition i _ _ _ _) -> i == cid) <$> scanRows app.condition
          forM_ existing (deleteRow app.condition)
          deleteWhere (\(Judgment _ c _ _ _ _ _ _ _) -> c == cid)
      , recordJudgment = \a -> do
          ReducerContext {timestamp = now} <- ask
          let k = judgmentKey a.condition a.rowKey
          deleteWhere (\(Judgment k' _ _ _ _ _ _ _ _) -> k' == k)
          insertRow
            app.judgment
            (Judgment k a.condition a.rowKey a.rowHash a.value a.label a.confidence a.model now)
      , forgetRow = \(ForgetRowArgs t r) -> do
          -- Row keys are only unique within a table: drop judgments under that table's conditions.
          cids <- map (\(Condition i _ _ _ _) -> i) . filter (\(Condition _ t' _ _ _) -> t' == t) <$> scanRows app.condition
          deleteWhere (\(Judgment _ c r' _ _ _ _ _ _) -> r' == r && c `elem` cids)
      }
