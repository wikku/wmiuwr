{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
module StreamTrans where
import qualified Data.Char (toLower)
import Data.Functor
import Control.Monad (ap, foldM)


data StreamTrans i o a
  = Return a
  | ReadS (Maybe i -> StreamTrans i o a)
  | WriteS o (StreamTrans i o a)
  deriving Functor

instance Applicative (StreamTrans i o) where
  pure = Return
  (<*>) = ap

instance Monad (StreamTrans i o) where
  return = Return
  (Return a) >>= f   = f a
  (ReadS k) >>= f    = ReadS $ (>>= f) <$> k
  (WriteS o s) >>= f = WriteS o (s >>= f)

toLower :: StreamTrans Char Char a
toLower = ReadS (\(Just c) -> WriteS (Data.Char.toLower c) toLower)

loop :: StreamTrans i o a
loop = ReadS (const loop)

runIOStreamTrans :: StreamTrans Char Char a -> IO a
runIOStreamTrans = \case
  Return a   -> return a
  ReadS f    -> getChar <&> Just <&> f >>= runIOStreamTrans
  WriteS c s -> putChar c >> runIOStreamTrans s

listTrans :: StreamTrans i o a -> [i] -> ([o], a)
listTrans (Return a) is    = ([], a)
listTrans (ReadS f) []     = listTrans (f Nothing) []
listTrans (ReadS f) (i:is) = listTrans (f (Just i)) is
listTrans (WriteS c s) is  = let (os, a) = listTrans s is in (c:os, a)

runCycle :: StreamTrans a a b -> b
runCycle st = b
  where (s, b) = listTrans st s

(|>|) :: StreamTrans i m a -> StreamTrans m o b -> StreamTrans i o b
s1 |>| (Return b) = Return b
s1 |>| (WriteS o s2) = WriteS o (s1 |>| s2)
(WriteS m s1) |>| (ReadS f) = s1 |>| f (Just m)
(ReadS f1) |>| s2 = ReadS $ \mi -> f1 mi |>| s2
(Return a) |>| (ReadS f) = Return a |>| f Nothing

catchOutput :: StreamTrans i o a -> StreamTrans i b (a, [o])
catchOutput (Return a)   = Return (a, [])
catchOutput (ReadS f)    = ReadS (catchOutput . f)
catchOutput (WriteS o s) = (fmap . fmap) (o:) (catchOutput s)

data BF
  = MoveR
  | MoveL
  | Inc
  | Dec
  | Output
  | Input
  | While [BF]
  deriving Show

brainfuckParser :: StreamTrans Char BF ()
brainfuckParser = ReadS $ \case
  Just '>' -> WriteS MoveR brainfuckParser
  Just '<' -> WriteS MoveL brainfuckParser
  Just '+' -> WriteS Inc brainfuckParser
  Just '-' -> WriteS Dec brainfuckParser
  Just '.' -> WriteS Output brainfuckParser
  Just ',' -> WriteS Input brainfuckParser
  Just ']' -> Return ()
  Nothing  -> Return ()
  Just '[' ->
    do while <- While <$> snd <$> catchOutput brainfuckParser
       WriteS while brainfuckParser
  Just _   -> brainfuckParser


runBF :: [BF] -> StreamTrans Char Char ()
runBF bfs = evalBFBlock ([], repeat 0) bfs >> return ()

coerceEnum :: (Enum a, Enum b) => a -> b
coerceEnum = toEnum . fromEnum

type Tape = ([Integer], [Integer])

evalBFBlock :: Tape -> [BF] -> StreamTrans Char Char Tape
evalBFBlock = foldM evalBF

evalBF :: Tape -> BF -> StreamTrans Char Char Tape
evalBF (l:ls, r:rs) MoveR  = Return (r:l:ls, rs)
evalBF (l:ls, r:rs) MoveL  = Return (ls, l:r:rs)
evalBF (ls, cur:rs) Inc    = Return (ls, (cur+1):rs)
evalBF (ls, cur:rs) Dec    = Return (ls, (cur-1):rs)
evalBF t@(l, cur:r) Output = WriteS (coerceEnum cur) (Return t)
evalBF (ls, cur:rs) Input  = ReadS $ \(Just c) -> Return (ls, coerceEnum c:rs)
evalBF t (While bfs)       = evalBFBlock t bfs


--

main = runIOStreamTrans toLower



