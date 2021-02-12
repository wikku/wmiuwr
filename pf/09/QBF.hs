module QBF where

type Var = String
data Formula
  = Var Var
  | Bot
  | Not Formula
  | And Formula Formula
  | All Var     Formula


type Env = Var -> Bool
eval :: Env -> Formula -> Bool
eval _ Bot = False
eval e (Var v) = e v
eval e (Not f) = not $ eval e f
eval e (And f1 f2) = eval e f1 && eval e f2
eval e (All b f) = eval et f && eval ef f
  where
    et v | v == b = True
         | otherwise = e v
    ef v | v == b = False
         | otherwise = e v

isTrue :: Formula -> Bool
isTrue = eval error
