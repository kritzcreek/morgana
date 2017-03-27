import Test.Tasty
import Test.Tasty.Hspec

import Morgana
import Morgana.Types

main = do
  hspecTests <- testSpec "Specs" hspecs
  let tests = testGroup "Tests" [hspecTests]
  defaultMain tests


hspecs = do
  describe "Rename refactoring" $ do
    it "renames a single identifier" $ do
      1 `shouldBe` 1

