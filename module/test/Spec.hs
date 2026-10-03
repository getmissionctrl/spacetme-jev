module Main (main) where

import Data.ByteString (ByteString)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Word (Word32)
import FakeBackend (backendFor, newFake, rowsOf)
import JevSchema
import SpacetimeDB.BSATN.Decoder (runExact)
import SpacetimeDB.BSATN.Encoder (runEncoder)
import SpacetimeDB.BSATN.Types (Timestamp (..))
import SpacetimeDB.Server (dispatchReducer, mkContext, reducerName, tableName)
import SpacetimeDB.Server.HKD (View (..))
import SpacetimeDB.Server.SpacetimeType (SpacetimeType (..))
import Test.Hspec

type Fake = IORef (Map Word32 [ByteString])

-- Reducer ids follow App field order.
call :: (SpacetimeType a) => Fake -> Int -> a -> IO (Either Text ())
call ref rid args = dispatchReducer jevModule rid (mkContext 0 0 0 0 0 0 42) (runEncoder encodeVal args) (backendFor ref)

addC, removeC, record, forget :: Int
addC = 0
removeC = 1
record = 2
forget = 3

judgments :: Fake -> IO [Judgment 'Value]
judgments ref = map (either (error . show) id . runExact (decodeVal @(Judgment 'Value))) <$> rowsOf ref "judgment"

conditions :: Fake -> IO [Condition 'Value]
conditions ref = map (either (error . show) id . runExact (decodeVal @(Condition 'Value))) <$> rowsOf ref "condition"

main :: IO ()
main = hspec $ describe "jev module" $ do
  it "derives wire names" $ do
    tableName app.condition `shouldBe` "condition"
    tableName app.judgment `shouldBe` "judgment"
    reducerName app.addCondition `shouldBe` "add_condition"
    reducerName app.recordJudgment `shouldBe` "record_judgment"
    reducerName app.forgetRow `shouldBe` "forget_row"

  it "add_condition validates kind and options" $ do
    ref <- newFake
    call ref addC (AddConditionArgs "c" "lead" "fits?" "maybe" "") `shouldReturn` Left "kind must be one of noul, choice, score"
    call ref addC (AddConditionArgs "c" "lead" "which?" "choice" "only-one") `shouldReturn` Left "choice/score need at least two options"
    call ref addC (AddConditionArgs "c" "lead" "which?" "choice" "a\n\nb\n") `shouldReturn` Right ()
    conditions ref `shouldReturn` [Condition "c" "lead" "which?" "choice" "a\nb"]

  it "record_judgment upserts one row per (condition, row)" $ do
    ref <- newFake
    _ <- call ref addC (AddConditionArgs "c" "lead" "fits?" "noul" "")
    _ <- call ref record (RecordJudgmentArgs "c" "1" "h1" 0.2 Nothing 0.8 "m")
    _ <- call ref record (RecordJudgmentArgs "c" "1" "h2" 0.7 Nothing 0.7 "m")
    _ <- call ref record (RecordJudgmentArgs "c" "2" "h3" 0.9 Nothing 0.9 "m")
    js <- judgments ref
    [(r, h, v) | Judgment _ _ r h v _ _ _ _ <- js] `shouldMatchList` [("1", "h2", 0.7), ("2", "h3", 0.9)]
    [t | Judgment _ _ _ _ _ _ _ _ t <- js] `shouldBe` [Timestamp 42, Timestamp 42]

  it "forget_row only touches the named table's conditions" $ do
    ref <- newFake
    _ <- call ref addC (AddConditionArgs "a" "lead" "x" "noul" "")
    _ <- call ref addC (AddConditionArgs "b" "ticket" "y" "noul" "")
    _ <- call ref record (RecordJudgmentArgs "a" "1" "h" 0.5 Nothing 0.5 "m")
    _ <- call ref record (RecordJudgmentArgs "b" "1" "h" 0.5 Nothing 0.5 "m")
    _ <- call ref forget (ForgetRowArgs "lead" "1")
    js <- judgments ref
    [c | Judgment _ c _ _ _ _ _ _ _ <- js] `shouldBe` ["b"]

  it "re-adding or removing a condition drops its judgments" $ do
    ref <- newFake
    _ <- call ref addC (AddConditionArgs "a" "lead" "x" "noul" "")
    _ <- call ref record (RecordJudgmentArgs "a" "1" "h" 0.5 Nothing 0.5 "m")
    _ <- call ref addC (AddConditionArgs "a" "lead" "x, reworded" "noul" "")
    judgments ref `shouldReturn` []
    _ <- call ref record (RecordJudgmentArgs "a" "1" "h" 0.5 Nothing 0.5 "m")
    _ <- call ref removeC (RemoveConditionArgs "a")
    judgments ref `shouldReturn` []
    conditions ref `shouldReturn` []
