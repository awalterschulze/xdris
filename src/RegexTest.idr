module RegexTest

import Specdris.Spec
import Regex
import Data.SortedMap

%access export

testSpec : IO ()
testSpec = spec $ do
  describe "nullable test" $ do
    it "Empty" $ do
      (nullExpr Empty) `shouldBe` True
    it "Or Char a Char b" $ do
      (nullExpr (Or (Letter 'a') (Letter 'b'))) `shouldNotBe` True
    it "todo" $ do
      pendingWith "do this later"
  describe "deriv test" $ do
    it "first deriv" $ do
      (derivPat (ZeroOrMore 
          (Choice 
            (Choice
              (mkVar "x" (Letter 'A'))
              (mkVar "y" (Concat (Letter 'A') (Letter 'B')))
            )
            (mkVar "z" (Letter 'B'))
          )
        ) 'A'
      ) `shouldBe` (mkVar "x" Empty)
    it "derive two" $ do
      (foldl derivPat (ZeroOrMore 
          (Choice 
            (Choice
              (mkVar "x" (Letter 'A'))
              (mkVar "y" (Concat (Letter 'A') (Letter 'B')))
            )
            (mkVar "z" (Letter 'B'))
          )
        ) (unpack "AB")
      ) `shouldBe` (mkVar "x" Empty)
  describe "env" $ do
    it "one match" $ do
      (env (VarsBase "x" "ABC" Empty)) `shouldBe` [insert "x" "ABC" empty]
    it "choice match" $ do
      (env $ 
        Choice (VarsBase "x" "ABC" Empty) (VarsBase "y" "CDE" Empty)
      ) `shouldBe` [insert "x" "ABC" empty, insert "y" "CDE" empty]
    it "pair match" $ do
      (env $ 
        Pair (VarsBase "x" "ABC" Empty) (VarsBase "y" "CDE" Empty)
      ) `shouldBe` [insert "y" "CDE" $ insert "x" "ABC" empty]
    it "choice match and no match" $ do
      (env $ 
        Choice (VarsBase "x" "ABC" Empty) (VarsBase "y" "CDE" EmptySet)
      ) `shouldBe` [insert "x" "ABC" empty]
    it "choice no match and match" $ do
      (env $ 
        Choice (VarsBase "x" "ABC" EmptySet) (VarsBase "y" "CDE" Empty)
      ) `shouldBe` [insert "y" "CDE" empty]
    it "zero or more match" $ do
      (env $ 
        ZeroOrMore (VarsBase "x" "ABC" Empty)
      ) `shouldBe` [insert "x" "ABC" empty]
    it "(x|A:ε+y|A:εB)+z|A:∅)" $ do
      (env 
        (Choice
          (Choice
            (VarsBase "x" "A" Empty)
            (VarsBase "y" "A" (Concat Empty (Letter 'B')))
          )
          (VarsBase "z" "A" EmptySet)
        )
      ) `shouldBe` [insert "x" "A" empty]
    it "(x|A:ε)(x|:A)*" $ do
      (env 
        (Pair
          (VarsBase "x" "A" Empty)
          (ZeroOrMore
            (VarsBase "x" "" (Letter 'A'))
          )
        )
      ) `shouldBe` [insert "x" "A" empty]
    it "((x|A:ε+y|A:εB)+z|A:∅)(((x|:A+y|:AB)+z|:B))*" $ do
      (env 
        (Pair
          (Choice
            (Choice
              (VarsBase "x" "A" Empty)
              (VarsBase "y" "A" (Concat Empty (Letter 'B')))
            )
            (VarsBase "z" "A" EmptySet)
          )
          (ZeroOrMore
            (Choice
              (Choice
                (VarsBase "x" "" (Letter 'A'))
                (VarsBase "y" "" (Concat (Letter 'A') (Letter 'B')))
              )
              (VarsBase "z" "" (Letter 'B'))
            )
          )
        )
      ) `shouldBe` [insert "x" "A" empty]
  describe "match test" $ do
    it "first match" $ do
      (match (ZeroOrMore 
          (Choice 
            (Choice
              (mkVar "x" (Letter 'A'))
              (mkVar "y" (Concat (Letter 'A') (Letter 'B')))
            )
            (mkVar "z" (Letter 'B'))
          )
        ) "A"
      ) `shouldBe` []
