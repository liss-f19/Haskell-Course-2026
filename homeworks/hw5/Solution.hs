import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)
import Text.Read (readMaybe)

-- Task 1. **Stack machine**

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG
  deriving Show

execInstr :: Instr -> State [Int] ()
execInstr (PUSH x) = modify (x :)

execInstr POP = do
  st <- get
  case st of
    (_:xs) -> put xs
    []     -> return ()

execInstr DUP = do
  st <- get
  case st of
    (x:xs) -> put (x:x:xs)
    []     -> return ()

execInstr SWAP = do
  st <- get
  case st of
    (x:y:xs) -> put (y:x:xs)
    _        -> return ()

execInstr ADD = do
  st <- get
  case st of
    (x:y:xs) -> put ((x + y) : xs)
    _        -> return ()

execInstr MUL = do
  st <- get
  case st of
    (x:y:xs) -> put ((x * y) : xs)
    _        -> return ()

execInstr NEG = do
  st <- get
  case st of
    (x:xs) -> put ((-x) : xs)
    []     -> return ()

execProg :: [Instr] -> State [Int] ()
execProg []     = return ()
execProg (i:is) = do
  execInstr i
  execProg is

runProg :: [Instr] -> [Int]
runProg prog = execState (execProg prog) []


-- Task 2. **Expression evaluator with variable bindings**

data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Assign String Expr
  | Seq Expr Expr
  deriving Show

eval :: Expr -> State (Map String Int) Int
eval (Num n) = return n

eval (Var name) = do
  env <- get
  return (env Map.! name)

eval (Add e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 + v2)

eval (Mul e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 * v2)

eval (Neg e) = do
  v <- eval e
  return (-v)

eval (Assign name e) = do
  v <- eval e
  modify (Map.insert name v)
  return v

eval (Seq e1 e2) = do
  _ <- eval e1
  eval e2

runEval :: Expr -> Int
runEval e = evalState (eval e) Map.empty


-- Task 3. **Memoised edit (Levenshtein) distance**

editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
  cache <- get
  case Map.lookup (i, j) cache of
    Just value -> return value
    Nothing -> do
      result <-
        if i == 0 then
          return j
        else if j == 0 then
          return i
        else if xs !! (i - 1) == ys !! (j - 1) then
          editDistM xs ys (i - 1) (j - 1)
        else do
          deletion <- editDistM xs ys (i - 1) j
          insertion <- editDistM xs ys i (j - 1)
          substitution <- editDistM xs ys (i - 1) (j - 1)

          return (1 + minimum [deletion, insertion, substitution])

      modify (Map.insert (i, j) result)
      return result

editDistance :: String -> String -> Int
editDistance xs ys =
  evalState (editDistM xs ys (length xs) (length ys)) Map.empty


-- task 4. **Player movement and decisions**

data LocationType
  = Normal
  | Decision [String]
  | Obstacle Int
  | Treasure Int
  | Trap Int
  | Goal
  deriving Show

data GameState = GameState
  { position :: Int
  , energy   :: Int
  , score    :: Int
  , pathName :: String
  , board    :: Map Int LocationType
  } deriving Show

type AdventureGame a = StateT GameState IO a


-- board

initialBoard :: Map Int LocationType
initialBoard = Map.fromList
  [ (0, Normal)
  , (2, Treasure 10)
  , (3, Decision ["forest", "cave", "river"])
  , (5, Obstacle 2)
  , (6, Treasure 20)
  , (8, Trap 15)
  , (10, Treasure 30)
  , (12, Obstacle 3)
  , (14, Trap 10)
  , (15, Goal)
  ]

initialState :: GameState
initialState = GameState
  { position = 0
  , energy = 20
  , score = 0
  , pathName = "start"
  , board = initialBoard
  }



movePlayer :: Int -> AdventureGame Int
movePlayer roll = do
  st <- get

  let currentPos = position st
      newPos = min 15 (currentPos + roll)
      newEnergy = energy st - 1

  put st
    { position = newPos
    , energy = newEnergy
    }

  return (newPos - currentPos)


makeDecision :: [String] -> AdventureGame String
makeDecision options = do
  choice <- lift (getPlayerChoice options)

  st <- get
  put st { pathName = choice }

  case choice of
    "forest" -> modify (\s -> s { score = score s + 5 })
    "cave"   -> modify (\s -> s { energy = energy s - 2 })
    "river"  -> modify (\s -> s { position = position s + 1 })
    _        -> return ()

  return choice

-- Task 5. **Game loop**

handleLocation :: AdventureGame Bool
handleLocation = do
  st <- get

  let pos = position st
      loc = Map.findWithDefault Normal pos (board st)

  case loc of
    Normal -> do
      lift $ putStrLn "You are on a normal path."
      return False

    Decision options -> do
      lift $ putStrLn "You reached a decision point!"
      _ <- makeDecision options
      return False

    Obstacle damage -> do
      lift $ putStrLn ("Obstacle! You lose " ++ show damage ++ " energy.")
      modify (\s -> s { energy = energy s - damage })
      return False

    Treasure points -> do
      lift $ putStrLn ("Treasure found! You gain " ++ show points ++ " points.")
      modify (\s -> s { score = score s + points })
      return False

    Trap penalty -> do
      lift $ putStrLn ("Trap! You lose " ++ show penalty ++ " points.")
      modify (\s -> s { score = max 0 (score s - penalty) })
      return False

    Goal -> do
      lift $ putStrLn "You reached the main treasure!"
      return True


-- turn

playTurn :: AdventureGame Bool
playTurn = do
  st <- get

  if energy st <= 0 then do
    lift $ putStrLn "You ran out of energy!"
    return True
  else do
    roll <- lift getDiceRoll
    moved <- movePlayer roll

    lift $ putStrLn ("You moved " ++ show moved ++ " spaces.")

    ended <- handleLocation

    newState <- get
    lift $ displayGameState newState

    if energy newState <= 0 then do
      lift $ putStrLn "Game over: no energy left."
      return True
    else
      return ended



playGame :: AdventureGame ()
playGame = do
  ended <- playTurn
  if ended
    then do
      st <- get
      lift $ putStrLn ("Final score: " ++ show (score st))
    else
      playGame


-- Task 6. **User interaction in `IO`**

getDiceRoll :: IO Int
getDiceRoll = do
  putStrLn "Enter dice roll result between 1 and 6:"
  input <- getLine

  case readMaybe input of
    Just n | n >= 1 && n <= 6 -> return n
    _ -> do
      putStrLn "Invalid dice roll. Try again."
      getDiceRoll

displayGameState :: GameState -> IO ()
displayGameState st = do
  putStrLn "----------------------------"
  putStrLn ("Position: " ++ show (position st))
  putStrLn ("Energy:   " ++ show (energy st))
  putStrLn ("Score:    " ++ show (score st))
  putStrLn ("Path:     " ++ pathName st)
  putStrLn "----------------------------"

getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do
  putStrLn "Choose one path:"
  mapM_ putStrLn options

  choice <- getLine

  if choice `elem` options
    then return choice
    else do
      putStrLn "Invalid choice. Try again."
      getPlayerChoice options



main :: IO ()
main = do
  putStrLn "Welcome to Treasure Hunters!"
  evalStateT playGame initialState