import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable (traverse)
import Data.List (permutations)
import Control.Monad (guard)
import Control.Monad.Writer (Writer, tell, runWriter)

-- Task 1. Maze navigation
-- A maze is represented as a map from positions to their neighbours in each direction:
type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)

-- Task 1a.
move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
  neighbours <- Map.lookup pos maze
  Map.lookup dir neighbours

-- Task 1b.
followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze pos []     = Just pos
followPath maze pos (d:ds) = do
  next <- move maze pos d
  followPath maze next ds

-- Task 1c.
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze pos []     = Just [pos]
safePath maze pos (d:ds) = do
  next  <- move maze pos d
  rest  <- safePath maze next ds
  return (pos : rest)

-- Task 2. Decoding a message

type Key = Map Char Char

-- Task 2a.
decrypt :: Key -> String -> Maybe String
decrypt key = traverse (`Map.lookup` key)

-- Task 2b.
decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)

-- Task 3.

type Guest    = String
type Conflict = (Guest, Guest)

-- Returns all valid round-table permutations with no conflicting neighbours
seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
  perm <- permutations guests
  guard (noConflicts perm)
  return perm
  where
    noConflicts []  = True
    noConflicts [_] = True
    noConflicts perm =
      let pairs = zip perm (tail perm) ++ [(last perm, head perm)]
      in all (not . isConflict) pairs

    isConflict (a, b) = (a, b) `elem` conflicts || (b, a) `elem` conflicts

-- Task 4. Result monad with warnings

data Result a = Failure String | Success a [String]
  deriving (Show)

-- Task 4a.

instance Functor Result where
  fmap _ (Failure msg)      = Failure msg
  fmap f (Success a ws)     = Success (f a) ws

instance Applicative Result where
  pure a = Success a []
  Failure msg    <*> _              = Failure msg
  _              <*> Failure msg    = Failure msg
  Success f ws1  <*> Success a ws2  = Success (f a) (ws1 ++ ws2)

instance Monad Result where
  return = pure
  Failure msg    >>= _ = Failure msg
  Success a ws   >>= f =
    case f a of
      Failure msg     -> Failure msg
      Success b ws'   -> Success b (ws ++ ws')

-- Task 4b.

warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure = Failure

-- Task 4c.

validateAge :: Int -> Result Int
validateAge age
  | age < 0   = failure "Age cannot be negative"
  | age > 150 = do warn "Age is above 150, are you sure?"; return age
  | otherwise  = return age

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge

-- Task 5.

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr
  deriving (Show)

-- alg simplification rules
simplify :: Expr -> Writer [String] Expr

-- Recurse into subtrees first, then simplify the root
simplify (Add l r) = do
  l' <- simplify l
  r' <- simplify r
  simplifyAdd l' r'

simplify (Mul l r) = do
  l' <- simplify l
  r' <- simplify r
  simplifyMul l' r'

simplify (Neg e) = do
  e' <- simplify e
  case e' of
    Neg inner -> do
      tell ["Double negation: -(-e) -> e"]
      return inner
    _ -> return (Neg e')

simplify e = return e

-- Helper: apply Add-level rules + logger
simplifyAdd :: Expr -> Expr -> Writer [String] Expr
simplifyAdd (Lit 0) e = do
  tell ["Add identity: 0 + e -> e"]
  return e
simplifyAdd e (Lit 0) = do
  tell ["Add identity: e + 0 -> e"]
  return e
simplifyAdd (Lit a) (Lit b) = do
  tell ["Constant folding: " ++ show a ++ " + " ++ show b ++ " -> " ++ show (a + b)]
  return (Lit (a + b))
simplifyAdd l r = return (Add l r)

-- Helper: apply Mul-level rules + logger
simplifyMul :: Expr -> Expr -> Writer [String] Expr
simplifyMul (Lit 0) _ = do
  tell ["Zero absorption: 0 * e -> 0"]
  return (Lit 0)
simplifyMul _ (Lit 0) = do
  tell ["Zero absorption: e * 0 -> 0"]
  return (Lit 0)
simplifyMul (Lit 1) e = do
  tell ["Mul identity: 1 * e -> e"]
  return e
simplifyMul e (Lit 1) = do
  tell ["Mul identity: e * 1 -> e"]
  return e
simplifyMul (Lit a) (Lit b) = do
  tell ["Constant folding: " ++ show a ++ " * " ++ show b ++ " -> " ++ show (a * b)]
  return (Lit (a * b))
simplifyMul l r = return (Mul l r)

-- Task 6. ZipList — an Applicative that is not a Monad

newtype ZipList a = ZipList { getZipList :: [a] } deriving (Show)

-- Task 6a.

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
  -- pure must produce an infinite list so it zips with any length list
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

-- Task 6b. Verification:
-- works

-- Task 6c. Why ZipList cannot have a lawful Monad instance:
-- Explain (in a comment) why `ZipList` cannot have a lawful `Monad` instance. 
-- Specifically, what goes wrong when you try to define `>>=`? 
-- Consider what happens when the function passed to `>>=` returns lists of different lengths.

-- Answer:
-- A lawful Monad instance is impossible because >>= would need to combine results from f applied to each element, 
-- but f can return ZipLists of different lengths. 
-- Anything we do with those lengths - truncate, pad, etc. - breaks the associativity law. 
-- The intermediate truncation from (m >>= f) >>= g doesn't equal m >>= (\x -> f x >>= g) 
-- because the length decisions happen at different points.

