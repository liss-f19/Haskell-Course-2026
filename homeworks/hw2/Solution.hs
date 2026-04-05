-- A Sequence a represents a sequence of values of type a. 
-- Empty is the empty sequence, 
-- Single x is a one-element sequence, 
-- and Append l r is the concatenation of two subsequences.

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
    deriving (Show)

-- Task 1. **Functor for Sequence**
instance Functor Sequence where
  fmap _ Empty        = Empty
  fmap f (Single x)   = Single (f x)
  fmap f (Append l r) = Append (fmap f l) (fmap f r)

-- Task 2. **Foldable for Sequence**
instance Foldable Sequence where
  foldMap _ Empty        = mempty
  foldMap f (Single x)   = f x
  foldMap f (Append l r) = foldMap f l <> foldMap f r

seqToList :: Sequence a -> [a]
seqToList = foldMap (:[])

seqLength :: Sequence a -> Int
seqLength = length

-- Task 3. **Semigroup and Monoid for Sequence**
instance Semigroup (Sequence a) where
  Empty <> x = x
  x <> Empty = x
  x <> y     = Append x y

instance Monoid (Sequence a) where
  mempty = Empty

-- Task 4. Tail Recursion and Sequence Search
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem target s = go [s]
  where
    go []           = False
    go (cur : rest) =
      case cur of
        Empty        -> go rest
        Single x     -> x == target || go rest
        Append l r   -> go (l : r : rest)

-- Task 5. Tail Recursion and Sequence Flatten
tailToList :: Sequence a -> [a]
tailToList s = reverse (go [s] [])
  where
    go [] acc           = acc
    go (cur : rest) acc =
      case cur of
        Empty      -> go rest acc
        Single x   -> go rest (x : acc)
        Append l r -> go (l : r : rest) acc

-- Task 5. Polisg RPN
data Token = TNum Int | TAdd | TSub | TMul | TDiv

tailRPN :: [Token] -> Maybe Int
tailRPN toks = go toks []
  where
    go []           [result] = Just result
    go []           _        = Nothing
    go (TNum n : ts) stack   = go ts (n : stack)
    go (t      : ts) stack   =
      case stack of
        (x : y : rest) ->
          case applyOp t y x of
            Nothing -> Nothing
            Just v  -> go ts (v : rest)
        _ -> Nothing

    applyOp TAdd a b = Just (a + b)
    applyOp TSub a b = Just (a - b)
    applyOp TMul a b = Just (a * b)
    applyOp TDiv a 0 = Nothing
    applyOp TDiv a b = Just (a `div` b)

-- Task 6 - **Expressing functions via `foldr` and `foldl`**
-- Task 6a
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- Task 6b
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr step []
  where
    step x acc
      | p x       = x : acc
      | otherwise = []

-- Task 6c
decimal :: [Int] -> Int
decimal = foldl (\acc d -> acc * 10 + d) 0

-- Task 7 - **Run-length encoding via folds**
-- Task 7a
encode :: Eq a => [a] -> [(a, Int)]
encode = foldr step []
  where
    step x [] = [(x, 1)]
    step x ((y, n) : rest)
      | x == y    = (y, n + 1) : rest   
      | otherwise = (x, 1) : (y, n) : rest

-- Task 7b
decode :: [(a, Int)] -> [a]
decode = foldr (\(x, n) acc -> replicate n x ++ acc) []

