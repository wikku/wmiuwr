{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Fmt2 where

data Format a b where
  Lit   :: String -> Format a a
  Int   :: Format a (Integer -> a)
  Str   :: Format a (String -> a)
  (:^:) :: Format b c -> Format a b -> Format a c

ksprintf :: Format a b -> (String -> a) -> b
ksprintf (Lit s) k     = k s
ksprintf Int k         = \i -> k (show i)
ksprintf Str k         = \s -> k s
ksprintf (f1 :^: f2) k = ksprintf f1 (\s1 -> ksprintf f2 (\s2 -> k (s1 ++ s2)))

test1 = Lit "Ala ma " :^: Int :^: Lit " kot" :^: Str :^: Lit "."

sprintf fmt = ksprintf fmt id

kprintf :: Format a b -> (IO () -> a) -> b
kprintf (Lit s) k     = k $ putStr s
kprintf Int k         = \i -> k (putStr $ show i)
kprintf Str k         = \s -> k (putStr s)
kprintf (f1 :^: f2) k = kprintf f1 (\o1 -> kprintf f2 (\o2 -> k (o1 >> o2)))

printf fmt = kprintf fmt id

