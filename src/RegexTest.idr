module RegexTest

import Specdris.Spec
import Regex

%access export

testSpec : IO ()
testSpec = spec $ do
  describe "nullable test" $ do
    it "Empty" $ do
      (Regex.nullable Regex.Empty) `shouldBe` True
    it "Or Char a Char b" $ do
      (Regex.nullable (Regex.Or (Regex.Symbol 'a') (Regex.Symbol 'b'))) `shouldNotBe` True
    it "todo" $ do
      pendingWith "do this later"