module MathLang.Eval
  ( Value(..)
  , EvalError(..)
  , evalProgram
  , evalExpr
  , addValue
  , subValue
  , mulValue
  , dotValue
  , transposeValue
  , identityMatrix
  ) where

import Control.Monad (foldM)
import qualified Data.Map.Strict as M
import MathLang.AST

data Value
  = Scalar Double
  | Vector [Double]
  | Matrix [[Double]]
  deriving (Eq, Show)

data EvalError
  = UnboundVariable String
  | UnknownFunction String
  | ArityMismatch String Int Int
  | TypeError String
  | DimensionMismatch String
  | EmptyProgram
  deriving (Eq, Show)

type VarEnv = M.Map String Value
type FunEnv = M.Map String ([String], Expr)

evalProgram :: Program -> Either EvalError Value
evalProgram (Program defs expr) = do
  (venv, fenv) <- loadDefs defs
  evalExpr venv fenv expr

loadDefs :: [Definition] -> Either EvalError (VarEnv, FunEnv)
loadDefs = foldM step (M.empty, M.empty)
  where
    step (venv, fenv) def =
      case def of
        VarDef name e -> do
          v <- evalExpr venv fenv e
          pure (M.insert name v venv, fenv)
        FuncDef name args body ->
          pure (venv, M.insert name (args, body) fenv)

evalExpr :: VarEnv -> FunEnv -> Expr -> Either EvalError Value
evalExpr venv fenv expr =
  case expr of
    Var n -> maybe (Left (UnboundVariable n)) Right (M.lookup n venv)
    ScalarLit d -> Right (Scalar d)
    VecLit xs -> do
      vals <- mapM (evalExpr venv fenv) xs
      nums <- mapM asScalar vals
      Right (Vector nums)
    MatLit rows -> do
      rows' <- mapM evalRow rows
      ensureRectangular rows'
      Right (Matrix rows')
    BinOp op a b -> do
      va <- evalExpr venv fenv a
      vb <- evalExpr venv fenv b
      case op of
        Add -> addValue va vb
        Sub -> subValue va vb
        Mul -> mulValue va vb
        Div -> divValue va vb
        Dot -> dotValue va vb
    Apply name args ->
      if name == "dot"
        then evalBuiltinDot venv fenv args
        else case M.lookup name fenv of
          Nothing -> Left (UnknownFunction name)
          Just (params, body)
            | length params /= length args -> Left (ArityMismatch name (length params) (length args))
            | otherwise -> do
                argVals <- mapM (evalExpr venv fenv) args
                let local = M.fromList (zip params argVals)
                evalExpr (M.union local venv) fenv body
    Let name rhs body -> do
      val <- evalExpr venv fenv rhs
      evalExpr (M.insert name val venv) fenv body
    Neg e -> do
      v <- evalExpr venv fenv e
      negateValue v
  where
    evalRow row = do
      vals <- mapM (evalExpr venv fenv) row
      mapM asScalar vals

asScalar :: Value -> Either EvalError Double
asScalar (Scalar d) = Right d
asScalar _ = Left (TypeError "expected scalar")

negateValue :: Value -> Either EvalError Value
negateValue (Scalar x) = Right (Scalar (-x))
negateValue (Vector xs) = Right (Vector (map negate xs))
negateValue (Matrix rows) = Right (Matrix (map (map negate) rows))

addValue :: Value -> Value -> Either EvalError Value
addValue (Scalar a) (Scalar b) = Right (Scalar (a + b))
addValue (Vector a) (Vector b)
  | length a == length b = Right (Vector (zipWith (+) a b))
  | otherwise = Left (DimensionMismatch "vector addition requires equal lengths")
addValue (Matrix a) (Matrix b)
  | sameShape a b = Right (Matrix (zipWith (zipWith (+)) a b))
  | otherwise = Left (DimensionMismatch "matrix addition requires equal dimensions")
addValue _ _ = Left (TypeError "addition requires same-shaped scalar/vector/matrix operands")

subValue :: Value -> Value -> Either EvalError Value
subValue (Scalar a) (Scalar b) = Right (Scalar (a - b))
subValue (Vector a) (Vector b)
  | length a == length b = Right (Vector (zipWith (-) a b))
  | otherwise = Left (DimensionMismatch "vector subtraction requires equal lengths")
subValue (Matrix a) (Matrix b)
  | sameShape a b = Right (Matrix (zipWith (zipWith (-)) a b))
  | otherwise = Left (DimensionMismatch "matrix subtraction requires equal dimensions")
subValue _ _ = Left (TypeError "subtraction requires same-shaped scalar/vector/matrix operands")

mulValue :: Value -> Value -> Either EvalError Value
mulValue (Scalar a) (Scalar b) = Right (Scalar (a * b))
mulValue (Scalar a) (Vector v) = Right (Vector (map (a *) v))
mulValue (Vector v) (Scalar a) = Right (Vector (map (* a) v))
mulValue (Scalar a) (Matrix m) = Right (Matrix (map (map (a *)) m))
mulValue (Matrix m) (Scalar a) = Right (Matrix (map (map (* a)) m))
mulValue (Matrix a) (Vector v)
  | null a = Left (DimensionMismatch "cannot multiply empty matrix")
  | length (head a) /= length v = Left (DimensionMismatch "matrix-vector multiplication dimension mismatch")
  | otherwise = Right (Vector (map (sum . zipWith (*) v) a))
mulValue (Matrix a) (Matrix b)
  | null a || null b = Left (DimensionMismatch "cannot multiply empty matrices")
  | length (head a) /= length b = Left (DimensionMismatch "matrix multiplication requires cols(A) == rows(B)")
  | otherwise =
      let bt = transpose b
       in Right (Matrix [[sum (zipWith (*) row col) | col <- bt] | row <- a])
mulValue _ _ = Left (TypeError "unsupported operands for multiplication")

divValue :: Value -> Value -> Either EvalError Value
divValue (Scalar a) (Scalar b) = Right (Scalar (a / b))
divValue _ _ = Left (TypeError "division is only defined for scalars")

dotValue :: Value -> Value -> Either EvalError Value
dotValue (Vector a) (Vector b)
  | length a == length b = Right (Scalar (sum (zipWith (*) a b)))
  | otherwise = Left (DimensionMismatch "dot product requires equal-length vectors")
dotValue _ _ = Left (TypeError "dot product requires vectors")

evalBuiltinDot :: VarEnv -> FunEnv -> [Expr] -> Either EvalError Value
evalBuiltinDot venv fenv [a, b] = do
  va <- evalExpr venv fenv a
  vb <- evalExpr venv fenv b
  dotValue va vb
evalBuiltinDot _ _ args = Left (ArityMismatch "dot" 2 (length args))

transposeValue :: Value -> Either EvalError Value
transposeValue (Matrix rows) = Right (Matrix (transpose rows))
transposeValue _ = Left (TypeError "transpose expects a matrix")

identityMatrix :: Int -> Value
identityMatrix n = Matrix [[if i == j then 1 else 0 | j <- [1 .. n]] | i <- [1 .. n]]

sameShape :: [[a]] -> [[b]] -> Bool
sameShape a b =
  length a == length b
    && and (zipWith (\x y -> length x == length y) a b)

ensureRectangular :: [[a]] -> Either EvalError ()
ensureRectangular [] = Left (DimensionMismatch "matrix must have at least one row")
ensureRectangular rows
  | any null rows = Left (DimensionMismatch "matrix rows must be non-empty")
  | all ((== cols) . length) rows = Right ()
  | otherwise = Left (DimensionMismatch "matrix rows must have equal length")
  where
    cols = length (head rows)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose rows
  | any null rows = []
  | otherwise = map head rows : transpose (map tail rows)
