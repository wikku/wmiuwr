{-# LaNgUaGe ScopedTypeVariables #-}
{-# LaNgUaGe TypeApplications #-}
import Data.List

class Finite a where
  elems :: [a]
  index :: a -> Integer

instance Finite Bool where
  elems = [False, True]
  index False = 0
  index True = 1

instance Finite a => Finite (Maybe a) where
  elems = Nothing : fmap Just elems
  index Nothing = 0
  index (Just a) = 1 + index a

instance (Finite a, Finite b) => Finite (a, b) where
  elems = [(a,b) | a <- elems, b <- elems]
  index (a,b) = (index a)*bLength + index b
    where bLength = toInteger $ length (elems :: [b])

instance (Finite a, Finite b) => Finite (a -> b) where
  elems = map toFunction $ sequence $ zipWith const (repeat elems) aElems
    where
      aElems = elems :: [a]
      toFunction xs = \a -> xs !! fromInteger (index a)
  index f = foldl' (\s i -> i + base * s) 0 $ map (index . f) elems
    where base = toInteger $ length (elems :: [b])


