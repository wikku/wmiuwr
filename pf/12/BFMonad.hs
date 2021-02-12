{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.Functor
import Control.Monad.Trans
import Control.Monad.Trans.State
import System.IO

data BF
  = MoveR
  | MoveL
  | Inc
  | Dec
  | Output
  | Input
  | While [BF]
  deriving Show

class Monad m => BFMonad m where
  moveLeft  :: m ()
  moveRight :: m ()
  tapeGet   :: m Integer
  tapePut   :: Integer -> m ()
  printChar :: Char -> m ()
  readChar  :: m Char

tapeModify f = tapePut . f =<< tapeGet

coerceEnum :: (Enum a, Enum b) => a -> b
coerceEnum = toEnum . fromEnum

evalBF :: BFMonad m => [BF] -> m ()
evalBF = mapM_ evalInst

evalInst :: BFMonad m => BF -> m ()
evalInst MoveL = moveLeft
evalInst MoveR = moveRight
evalInst Inc = tapeModify (+ 1)
evalInst Dec = tapeModify (subtract 1)
evalInst Output = tapeGet <&> coerceEnum >>= printChar
evalInst Input = readChar <&> coerceEnum >>= tapePut
evalInst (While bfs) = do
  i <- tapeGet
  if i == 0
    then return ()
    else evalBF bfs >> evalInst (While bfs)

data BFTape = BFTape [Integer] Integer [Integer]

newtype BFInterpreter a = BFI { unBFI :: StateT BFTape IO a }
  deriving (Functor, Applicative, Monad)

instance BFMonad BFInterpreter where
  tapeGet = BFI $ get <&> \(BFTape _ i _) -> i
  tapePut i = BFI $ get >>= \(BFTape l _ r) -> put (BFTape l i r)
  moveLeft = BFI $ get >>= \(BFTape (l:ls) m rs) -> put (BFTape ls l (m:rs))
  moveRight = BFI $ get >>= \(BFTape ls m (r:rs)) -> put (BFTape (m:ls) r rs)
  printChar c = BFI $ lift $ putChar c >> hFlush stdout
  readChar = BFI $ lift getChar

initTape = BFTape (repeat 0) 0 (repeat 0)

runBF :: [BF] -> IO ()
runBF bfs = runStateT (unBFI $ evalBF bfs) initTape >> return ()
