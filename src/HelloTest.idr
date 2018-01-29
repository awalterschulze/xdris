module HelloTest

import Specdris.Spec
import Hello

%access export

testSpec : IO ()
testSpec = spec $ do
  describe "This is my hello test" $ do
    it "checks hello world" $ do
      Hello.hello `shouldBe` "hello world"
    it "checks not hello" $ do
      Hello.hello `shouldNotBe` "hello not world"
    it "todo" $ do
      pendingWith "do this later"