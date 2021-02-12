{-# LANGUAGE DeriveFunctor #-}
import Control.Monad (ap)

type Symbol = String
data Term v
  = Var v
  | Sym Symbol [Term v]
  deriving (Show, Functor)

instance Applicative Term where
  pure  = return
  (<*>) = ap

instance Monad Term where
  return v = Var v
  (Var v) >>= f = f v
  (Sym s ts) >>= f = (Sym s (map (>>= f) ts))

