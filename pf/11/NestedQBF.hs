{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
module NestedQBF where
import Data.Void
import Control.Monad (ap)

data Formula v
  = Var v
  | Bot
  | Not (Formula v)
  | And (Formula v) (Formula v)
  | All (Formula (Inc v))
  deriving (Show, Functor)

instance Applicative Formula where
  pure = return
  (<*>) = ap

instance Monad Formula where
  return a = Var a
  ma >>= f = join $ fmap f ma

exFormula1 = All (Var Z `And` All (Var (S Z) `And` Var Z))

data Inc v = Z | S v
  deriving (Eq, Show, Functor)

join :: Formula (Formula v) -> Formula v
join Bot = Bot
join (Var v) = v
join (And f1 f2) = And (join f1) (join f2)
join (Not f) = Not (join f)
join (All f) = All $ join $ fmap swap f
  where
    swap :: Inc (Formula v) -> Formula (Inc v)
    swap Z = Var Z
    swap (S x) = fmap S x

subst :: Formula v -> Formula (Inc v) -> Formula v
subst t = join . fmap g
  where
    g Z = t
    g (S x) = Var x

