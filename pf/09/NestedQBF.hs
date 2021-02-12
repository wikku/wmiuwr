{-# LANGUAGE ScopedTypeVariables #-}
module NestedQBF where
import Data.Void

data Formula v
  = Var v
  | Bot
  | Not (Formula v)
  | And (Formula v) (Formula v)
  | All (Formula (Inc v))
  deriving Show

exFormula1 = All (Var Z `And` All (Var (S Z) `And` Var Z))
exFormula1 = All (Var Z `And` All (Var (S Z) `And` Var Z))

instance Functor Formula where
  fmap f (Var v) = Var (f v)
  fmap f Bot = Bot
  fmap f (Not v) = Not (fmap f v)
  fmap f (And v1 v2) = And (fmap f v1) (fmap f v2)
  fmap f (All v) = All (fmap (fmap f) v)

data Inc v = Z | S v
  deriving (Eq, Show)

instance Functor Inc where
  fmap f Z = Z
  fmap f (S x) = S (f x)

eval :: (a -> Bool) -> Formula a -> Bool
eval _ Bot = False
eval e (Var v) = e v
eval e (Not f) = not $ eval e f
eval e (And f1 f2) = eval e f1 && eval e f2
eval e (All f) = eval et f && eval ef f
  where
    et Z     = True
    et (S x) = e x
    ef Z     = False
    ef (S x) = e x

isTrue :: Formula Void -> Bool
isTrue = eval absurd

{-
subst :: (Eq v) => Formula v -> v -> Formula v -> Formula v
subst t x Bot = Bot
subst t x (Var v)
    | x == v     = t
    | otherwise  = (Var v)
subst t x (Not f) = Not (subst t x f)
subst t x (And f1 f2) = And (subst t x f1) (subst t x f2)
subst t x (All f) = All (subst (S <$> t) (S x) f)
-}

getS (S x) = x
getS Z = error "lol"

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

{-
subst :: forall v. Formula v -> Formula (Inc v) -> Formula v
subst t Bot = Bot
subst t (Var Z) = t
subst t (Var (S v)) = v
subst t (And f1 f2) = And (subst t f1) (subst t f2)
subst t (Not f) = Not (subst t f)
subst t (All f) = 
-}
subst :: Formula v -> Formula (Inc v) -> Formula v
subst t = join . fmap g
  where
    g Z = t
    g (S x) = Var x


isTrue' :: Formula Void -> Bool
isTrue' Bot = False
isTrue' (Var x) = absurd x
isTrue' (Not f) = not (isTrue' f)
isTrue' (And f1 f2) = isTrue' f1 && isTrue' f2
isTrue' (All f) = isTrue' (subst Bot f) && isTrue' (subst (Not Bot) f)
