{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}
import Data.Functor
import Control.Monad.State

data BF
  = MoveR
  | MoveL
  | Inc
  | Dec
  | Output
  | Input
  | While [BF]
  deriving Show

class Monad m => TapeMonad m a | m -> a where
  tapeGet   :: m a
  tapePut   :: a -> m ()
  moveLeft  :: m ()
  moveRight :: m ()

tapeModify f = tapePut . f =<< tapeGet

coerceEnum :: (Enum a, Enum b) => a -> b
coerceEnum = toEnum . fromEnum

evalBF' :: TapeMonad m Integer => [BF] -> [Char] -> ([Char] -> m [Char]) -> m [Char]
evalBF' [] is k = k is
evalBF' (MoveL:bfs) is k = moveLeft >> evalBF' bfs is k
evalBF' (MoveR:bfs) is k = moveRight >> evalBF' bfs is k
evalBF' (Inc:bfs) is k = tapeModify (+ 1) >> evalBF' bfs is k
evalBF' (Dec:bfs) is k = tapeModify (subtract 1) >> evalBF' bfs is k
evalBF' (Output:bfs) is k = do
  ch <- coerceEnum <$> tapeGet
  (ch:) <$> evalBF' bfs is k
evalBF' (Input:bfs) (i:is) k = tapePut (coerceEnum i) >> evalBF' bfs is k
evalBF' (While l:bfs) is k = do
  evalBF' l is k'
  where k' is = do
        val <- tapeGet
        case val of
          0 -> evalBF' bfs is k
          _ -> evalBF' l is k'

evalBF :: TapeMonad m Integer => [BF] -> [Char] -> m [Char]
evalBF bfs is = evalBF' bfs is (\_ -> return [])

data BFTape = BFTape [Integer] Integer [Integer]

instance TapeMonad (State BFTape) Integer where
  tapeGet = get <&> \(BFTape _ i _) -> i
  tapePut i = get >>= \(BFTape l _ r) -> put (BFTape l i r)
  moveLeft = get >>= \(BFTape (l:ls) m rs) -> put (BFTape ls l (m:rs))
  moveRight = get >>= \(BFTape ls m (r:rs)) -> put (BFTape (m:ls) r rs)

initTape = BFTape (repeat 0) 0 (repeat 0)

runBF :: [BF] -> [Char] -> [Char]
runBF bfs is = fst $ runState (evalBF bfs is) initTape
