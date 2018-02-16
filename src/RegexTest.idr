module RegexTest

import Specdris.Spec
import Regex

%access export

testSpec : IO ()
testSpec = spec $ do
  describe "nullable" $ do
    it "Empty" $ do
      (nullExpr Empty) `shouldBe` True
    it "a + b" $ do
      (nullExpr (Or (Letter 'a') (Letter 'b'))) `shouldNotBe` True
  describe "deriv" $ do
    it "(x|:A+y|:AB)+z|:B)* / A" $ do
      (derivPat (ZeroOrMore 
          (Choice 
            (Choice
              (mkVar "x" (Letter 'A'))
              (mkVar "y" (Concat (Letter 'A') (Letter 'B')))
            )
            (mkVar "z" (Letter 'B'))
          )
        ) 'A'
      ) `shouldBe` (Pair
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
  describe "env" $ do
    it "x|ABC:ε" $ do
      (env (VarsBase "x" "ABC" Empty)) `shouldBe` [mkEnv "x" "ABC"]
    it "(x|ABC:ε + y|CDE:ε)" $ do
      (env $ 
        Choice (VarsBase "x" "ABC" Empty) (VarsBase "y" "CDE" Empty)
      ) `shouldBe` [(mkEnv "x" "ABC") `append` (mkEnv "y" "CDE")]
    it "(x|ABC:ε , y|CDE:ε)" $ do
      (env $ 
        Pair (VarsBase "x" "ABC" Empty) (VarsBase "y" "CDE" Empty)
      ) `shouldBe` [(mkEnv "x" "ABC") `append` (mkEnv "y" "CDE")]
    it "(x|ABC:ε + y|CDE:∅)" $ do
      (env $ 
        Choice (VarsBase "x" "ABC" Empty) (VarsBase "y" "CDE" EmptySet)
      ) `shouldBe` [mkEnv "x" "ABC"]
    it "(x|ABC:∅ + y|CDE:ε)" $ do
      (env $ 
        Choice (VarsBase "x" "ABC" EmptySet) (VarsBase "y" "CDE" Empty)
      ) `shouldBe` [mkEnv "y" "CDE"]
    it "(x|ABC:ε)*" $ do
      (env $ 
        ZeroOrMore (VarsBase "x" "ABC" Empty)
      ) `shouldBe` [mkEnv "x" "ABC"]
    it "((x|A:ε+y|A:εB)+z|A:∅)" $ do
      (env 
        (Choice
          (Choice
            (VarsBase "x" "A" Empty)
            (VarsBase "y" "A" (Concat Empty (Letter 'B')))
          )
          (VarsBase "z" "A" EmptySet)
        )
      ) `shouldBe` [mkEnv "x" "A"]
    it "(x|A:ε)(x|:A)*" $ do
      (env 
        (Pair
          (VarsBase "x" "A" Empty)
          (ZeroOrMore
            (VarsBase "x" "" (Letter 'A'))
          )
        )
      ) `shouldBe` [mkEnv "x" "A"]
    it "((x|A:ε+y|A:εB)+z|A:∅)(((x|:A+y|:AB)+z|:B))*" $ do
      -- TODO: Are these brackets correct? or should they be (x + (y + z)) instead of ((x + y) + z)?
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
      ) `shouldBe` [mkEnv "x" "A"]
  describe "match" $ do
    it "((x|:A+y|:AB)+z|:B)* / A" $ do
      (match (ZeroOrMore 
          (Choice 
            (Choice
              (mkVar "x" (Letter 'A'))
              (mkVar "y" (Concat (Letter 'A') (Letter 'B')))
            )
            (mkVar "z" (Letter 'B'))
          )
        ) "A"
      ) `shouldBe` [mkEnv "x" "A"]
    it "((x|:A+y|:AB)+z|:B)* / AB" $ do
      (match (ZeroOrMore 
          (Choice 
            (Choice
              (mkVar "x" (Letter 'A'))
              (mkVar "y" (Concat (Letter 'A') (Letter 'B')))
            )
            (mkVar "z" (Letter 'B'))
          )
        ) "AB"
      ) `shouldBe` [(mkEnv "x" "A") `append` (mkEnv "z" "B"), mkEnv "y" "AB"]
    it "(xyz:((x|:A+y|:AB)+z|:B)*) / ABA" $ do
      (match (newVar "xyz" (ZeroOrMore 
          (Choice 
            (Choice
              (mkVar "x" (Letter 'A'))
              (mkVar "y" (Concat (Letter 'A') (Letter 'B')))
            )
            (mkVar "z" (Letter 'B'))
          )
        )) "ABA"
      ) `shouldBe` [(((mkEnv "xyz" "ABA") `append` (mkEnv "x" "A")) `append` (mkEnv "z" "B")) `append` (mkEnv "x" "A")
        , ((mkEnv "xyz" "ABA") `append` (mkEnv "y" "AB")) `append` (mkEnv "x" "A")]
    it "(xyz|:(xy|:(x|:A+AB,y|:BAA+A),z|:AC+C)) / ABAAC" $ do
      (match (newVar "xyz" (Pair
          (newVar "xy"
            (Pair
              (mkVar "x" (Or
                (Letter 'A')
                (Concat (Letter 'A') (Letter 'B'))
              ))
              (mkVar "y" (Or
                (Concat (Concat (Letter 'B') (Letter 'A')) (Letter 'A'))
                (Letter 'A')
              ))
            )
          ) (mkVar "z"
            (Or
              (Concat (Letter 'A') (Letter 'C'))
              (Letter 'C')
            )
          )
        )) "ABAAC"
      ) `shouldBe` [((((mkEnv "xyz" "ABAAC") `append` (mkEnv "xy" "ABAA")) `append` (mkEnv "x" "A")) `append` (mkEnv "y" "BAA")) `append` (mkEnv "z" "C")
        , ((((mkEnv "xyz" "ABAAC") `append` (mkEnv "xy" "ABA")) `append` (mkEnv "x" "AB")) `append` (mkEnv "y" "A")) `append` (mkEnv "z" "AC")]
