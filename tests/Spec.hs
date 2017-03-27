{-# LANGUAGE NoImplicitPrelude #-}
import Protolude
import Control.Lens
import Test.Tasty
import Test.Tasty.Hspec

import Morgana
import Morgana.Types
import Control.Concurrent.STM.TVar

main :: IO ()
main = do
  hspecTests <- testSpec "Specs" hspecs
  let tests = testGroup "Tests" [hspecTests]
  defaultMain tests

callMorgana
  :: SState
  -- ^ The starting state
  -> Text
  -- ^ The file contents `readFile` should return
  -> Command
  -- ^ The command to be executed
  -> IO ([Response], SState)
  -- ^ The responses sent as well as the resulting state
callMorgana st file c = do
  resRef <- newTVarIO []
  resState <- execStateT (commandProcessor (const (pure file)) (collectResponse resRef) c) st
  resResponse <- readTVarIO resRef
  pure (reverse resResponse, resState)
  where
    collectResponse ref = atomically . modifyTVar ref . (:)

-- TODO: Understand why the closing declaration is needed here unfortunately
moduleHeader :: (IsString m, Monoid m) => [m] -> m
moduleHeader ls = fold $ intersperse "\n" ("module MorganaSpec where" : ls ++ ["end = end"])

spanShouldBe :: SSpan -> SSpan -> IO ()
spanShouldBe sp1 sp2 = do
  sp1^.ssFile `shouldBe` sp2^.ssFile
  sp1^.ssStart `shouldBe` sp2^.ssStart
  sp1^.ssEnd `shouldBe` sp2^.ssEnd

hspecs :: Spec
hspecs = do
  describe "Selecting Spans" $ do
    it "selects a single var" $ do
      ([Span sp], _) <- callMorgana Waiting (moduleHeader ["x = var"]) (Pos "" 2 5)
      sp `spanShouldBe` (SSpan "" (SPos 2 5) (SPos 2 8))
  describe "Rename refactoring" $ do
    it "renames a single identifier" $ do
      () `shouldBe` ()

