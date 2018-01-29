module RegexTest

import Specdris.Spec
import Regex

%access export

testSpec : IO ()
testSpec = spec $ do
  describe "This is my hello test" $ do
    it "checks hello world" $ do
      Regex.hello `shouldBe` "hello world"
    it "checks not hello" $ do
      Regex.hello `shouldNotBe` "hello not world"
    it "todo" $ do
      pendingWith "do this later"