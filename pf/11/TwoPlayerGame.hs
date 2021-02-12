{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveFunctor #-}
import Data.List
import Control.Monad (ap)
import Data.Ord (Down(..), getDown)

class Monad m => TwoPlayerGame m s a b | m -> s a b where
  moveA :: s -> m a
  moveB :: s -> m b

data Score = AWins | Draw | BWins deriving (Show, Eq, Ord)


-- tic tac toe

data Square = Empty | A | B deriving (Eq)
instance Show Square where
  show A = "A"
  show B = "B"
  show Empty = "_"

data Board = Board [[Square]]
instance Show Board where
  show (Board xxs) = intercalate "\n" $ map (intercalate " " . map show) xxs

newtype AMove = AMove (Int, Int) deriving Read via (Int, Int)
newtype BMove = BMove (Int, Int) deriving Read via (Int, Int)

lineScore [A, A, A] = AWins
lineScore [B, B, B] = BWins
lineScore [_, _, _] = Draw

diag xxs = zipWith (!!) xxs [0..]

boardWinner (Board b) = find wins (map lineScore lines)
  where
    wins Draw = False
    wins _    = True
    horizontal   = b
    vertical     = transpose b
    diagonal     = diag b
    antidiagonal = diag (map reverse b)
    lines = diagonal : antidiagonal : horizontal ++ vertical

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt _ _ []     = []
modifyAt n f (x:xs)
  | n == 0 = f x : xs
  | n > 0  = x : modifyAt (n-1) f xs

placeSquare b x y s = modifyAt y (modifyAt x (const s)) b

emptyBoard = Board $ replicate 3 (replicate 3 Empty)

coordValid x = 0 <= x && x < 3
moveValid (x,y) (Board b) = coordValid x && coordValid y && b !! y !! x == Empty

ticTacToe :: TwoPlayerGame m Board AMove BMove => m Score
ticTacToe = go A emptyBoard
  where
    go A (Board b) = do
      AMove (x,y) <- moveA (Board b)
      if moveValid (x,y) (Board b) then
        let b' = placeSquare b x y A in
        case boardWinner (Board b') of
          Just x  -> return x
          Nothing -> go B (Board b')
      else return BWins
    go B (Board b) = do
      BMove (x,y) <- moveB (Board b)
      if moveValid (x,y) (Board b) then
        let b' = placeSquare b x y B in
        case boardWinner (Board b') of
          Just x  -> return x
          Nothing -> go A (Board b')
      else return AWins

newtype IOGame s a b x = IOGame { runIOGame :: IO x }
  deriving (Functor, Applicative, Monad) via IO

instance (Show s, Read a, Read b) => TwoPlayerGame (IOGame s a b) s a b where
  moveA s = IOGame $ print "A moves" >> print s >> readLn
  moveB s = IOGame $ print "B moves" >> print s >> readLn




data GameTree s a b x
  = Result x
  | AskA s (a -> GameTree s a b x)
  | AskB s (b -> GameTree s a b x)
  deriving (Functor)

instance Applicative (GameTree s a b) where
  pure = return
  (<*>) = ap

instance Monad (GameTree s a b) where
  return = Result
  (Result x) >>= f = f x
  (AskA s k) >>= f = AskA s $ (>>= f) <$> k
  (AskB s k) >>= f = AskB s $ (>>= f) <$> k


instance TwoPlayerGame (GameTree s a b) s a b where
  moveA s = AskA s return
  moveB s = AskB s return

optimalPlayScore
  :: Int
  -> (s -> [a])
  -> (s -> [b])
  -> GameTree s a b Score
  -> Score
optimalPlayScore _ _ _ (Result x) = x
optimalPlayScore 0 _ _ _ = Draw
optimalPlayScore d aMoves bMoves (AskA s k) =
  minimum $ map (optimalPlayScore (d-1) aMoves bMoves . k) (aMoves s)
optimalPlayScore d aMoves bMoves (AskB s k) =
  maximum $ map (optimalPlayScore (d-1) aMoves bMoves . k) (bMoves s)

-- argmax
maximumOn :: (Ord b) => (a -> b) -> [a] -> a
maximumOn f as = head $ sortOn (Down . f) as

play :: (Show s, Read a)
     => Int
     -> (s -> [a])
     -> (s -> [b])
     -> GameTree s a b Score
     -> IO ()
play _ _ _ (Result x) = print x
play depth aMoves bMoves (AskA s k) = do
  print "Game state:"
  print s
  aMove <- readLn
  play depth aMoves bMoves (k aMove)
play depth aMoves bMoves gt@(AskB s k) = do
  let bs = bMoves s
  let bMove =
       maximumOn (\b -> optimalPlayScore depth aMoves (const [b]) gt) bs
  play depth aMoves bMoves (k bMove)
