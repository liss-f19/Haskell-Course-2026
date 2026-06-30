module Main (main) where

import MathLang
import MathLang.Eval
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  parserSpec
  evaluatorSpec
  endToEndSpec
  propertySpec

parserSpec :: Spec
parserSpec = describe "Parser" $ do
  it "parses function definitions and expression" $ do
    let input = unlines
          [ "def f(x, y) = x + y"
          , "f(2, 3)"
          ]
    parseProgram input `shouldSatisfy` isRight

  it "respects precedence: multiplication binds tighter than addition" $ do
    let input = "1 + 2 * 3"
    let expected = Program [] (BinOp Add (ScalarLit 1) (BinOp Mul (ScalarLit 2) (ScalarLit 3)))
    parseProgram input `shouldBe` Right expected

  it "supports line comments" $ do
    let input = unlines
          [ "// this is a comment"
          , "1 + 2"
          ]
    parseProgram input `shouldSatisfy` isRight

  it "returns parse errors with location" $ do
    let input = "def f(x = x + 1"
    case parseProgram input of
      Left msg -> msg `shouldContain` "MathLang"
      Right _ -> expectationFailure "expected parse failure"

evaluatorSpec :: Spec
evaluatorSpec = describe "Evaluator" $ do
  it "evaluates scalar arithmetic" $ do
    evalText "1 + 2 * 3" `shouldBe` Right (Scalar 7)

  it "evaluates vector addition" $ do
    evalText "[1,2,3] + [4,5,6]" `shouldBe` Right (Vector [5, 7, 9])

  it "reports dimension mismatch" $ do
    evalText "[[1,2],[3,4]] * [1,2,3]"
      `shouldBe` Left (DimensionMismatch "matrix-vector multiplication dimension mismatch")

  it "checks associativity for matrix addition on fixed case" $ do
    let a = Matrix [[1, 2], [3, 4]]
    let b = Matrix [[5, 6], [7, 8]]
    let c = Matrix [[9, 10], [11, 12]]
    left <- addValue a b >>= (`addValue` c)
    right <- addValue b c >>= addValue a
    left `shouldBe` right

  it "checks distributive law on fixed case" $ do
    let a = Matrix [[1, 2], [3, 4]]
    let b = Matrix [[2, 0], [1, 2]]
    let c = Matrix [[0, 1], [2, 3]]
    left <- addValue b c >>= mulValue a
    ab <- mulValue a b
    ac <- mulValue a c
    right <- addValue ab ac
    left `shouldBe` right

endToEndSpec :: Spec
endToEndSpec = describe "End-to-end" $ do
  it "runs the regression-style sample" $ do
    let src = unlines
          [ "def loss(X, y, theta) ="
          , "  let preds = X * theta in"
          , "  let errors = preds - y in"
          , "  (1 / 2) * dot(errors, errors)"
          , ""
          , "loss([[1,2],[1,3],[1,4]], [2,3.5,4.8], [0,0])"
          ]
    evalText src `shouldBe` Right (Scalar 19.645)

  it "uses top-level variable definition" $ do
    let src = unlines
          [ "x = [1,2,3]"
          , "dot(x, x)"
          ]
    evalText src `shouldBe` Right (Scalar 14)

propertySpec :: Spec
propertySpec = describe "Property tests" $ do
  it "transpose is involutive" $ property $ \(SmallMat m) ->
    let v = Matrix m
        t1 = transposeValue v
        t2 = t1 >>= transposeValue
     in t2 == Right v

  it "transpose of product reverses order" $ property $ \(CompatMats a b) ->
    let va = Matrix a
        vb = Matrix b
        lhs = mulValue va vb >>= transposeValue
        rhs = transposeValue vb >>= \tb -> transposeValue va >>= mulValue tb
     in lhs == rhs

  it "matrix addition is commutative" $ property $ \(SameShapeMats a b) ->
    addValue (Matrix a) (Matrix b) == addValue (Matrix b) (Matrix a)

  it "identity matrix is left and right unit" $ property $ \(SquareMat a) ->
    let n = length a
        i = identityMatrix n
        va = Matrix a
        left = mulValue i va
        right = mulValue va i
     in left == Right va && right == Right va

newtype SmallMat = SmallMat [[Double]] deriving (Show)
newtype SquareMat = SquareMat [[Double]] deriving (Show)
data CompatMats = CompatMats [[Double]] [[Double]] deriving (Show)
data SameShapeMats = SameShapeMats [[Double]] [[Double]] deriving (Show)

instance Arbitrary SmallMat where
  arbitrary = do
    r <- chooseInt (1, 3)
    c <- chooseInt (1, 3)
    rows <- vectorOf r (vectorOf c smallNum)
    pure (SmallMat rows)

instance Arbitrary SquareMat where
  arbitrary = do
    n <- chooseInt (1, 3)
    rows <- vectorOf n (vectorOf n smallNum)
    pure (SquareMat rows)

instance Arbitrary CompatMats where
  arbitrary = do
    r <- chooseInt (1, 3)
    k <- chooseInt (1, 3)
    c <- chooseInt (1, 3)
    a <- vectorOf r (vectorOf k smallNum)
    b <- vectorOf k (vectorOf c smallNum)
    pure (CompatMats a b)

instance Arbitrary SameShapeMats where
  arbitrary = do
    r <- chooseInt (1, 3)
    c <- chooseInt (1, 3)
    a <- vectorOf r (vectorOf c smallNum)
    b <- vectorOf r (vectorOf c smallNum)
    pure (SameShapeMats a b)

smallNum :: Gen Double
smallNum = choose (-5, 5)

evalText :: String -> Either EvalError Value
evalText src = do
  prog <- mapLeft (TypeError . ("parse: " <>)) (parseProgram src)
  evalProgram prog

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f e = case e of
  Left x -> Left (f x)
  Right y -> Right y

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False
