{-# LANGUAGE DeriveFunctor #-}

import Control.Monad (ap)
import Control.Monad.State

posRem x y = case x `rem` y of
                 r | r < 0     -> r + y
                 r | otherwise -> r

intToPos i = if i < 0 then i + 2147483647 else i

class Monad m => Random m where
  random :: m Int

select n xs = let (ys, z:zs) = splitAt n xs in (z, ys++zs)

shuffle :: Random m => [a] -> m [a]
shuffle xs = go xs (length xs)
  where
    go [] _ = return []
    go xs l = do
        idx <- random
        let idx' = idx `posRem` l
        let (hd,tl) = select idx' xs
        tl' <- go tl (l-1)
        return (hd:tl')

newtype RS a = RS { unRS :: Int -> (Int, a) }
  deriving Functor

instance Applicative RS where
  pure  = return
  (<*>) = ap

instance Monad RS where
  return a = RS $ \i -> (i, a)
  ma >>= k = RS $ \i -> let (i', a) = unRS ma i in unRS (k a) i'

instance Random RS where
  random = RS $ \i -> let a = nextRand i in (a,a)


withSeed :: RS a -> Int -> a
withSeed (RS f) i = snd $ f i

nextRand a = let (d,m) = divMod a 127773 in intToPos $ 16807*m - 2836*d

-- withSeed (shuffle [1,2,3,4,5,6]) 123456



