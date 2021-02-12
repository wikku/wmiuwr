{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Maybe
import Data.Functor
import Control.Monad
import Data.Proxy

data RegExp a
  = Eps
  | Lit  (a -> Bool)
  | Or   (RegExp a) (RegExp a)
  | Cat  (RegExp a) (RegExp a)
  | Star (RegExp a)


match :: MonadPlus m => RegExp a -> [a] -> m (Maybe [a])
match Eps as = return Nothing
match (Lit f) [] = mzero
match (Lit f) (a:as) = if f a then return (Just as) else mzero
match (Or r1 r2) as = match r1 as `mplus` match r2 as
match (Cat r1 r2) as = match r1 as <&> fromMaybe as >>= match r2
match (Star r) as = matchSome `mplus` matchNone
  where matchSome = match r as >>= \case
                      Nothing  -> return Nothing
                      Just as' -> maybe (Just as') Just <$> match (Star r) as'
        matchNone = return Nothing

matchesWholeL :: RegExp a -> [a] -> Bool
matchesWholeL r [] = any isNothing $ match @[] r []
matchesWholeL r as = any ((== 0) . length) $ match @[] r as

matchesWholeM :: RegExp a -> [a] -> Bool
matchesWholeM r [] = any isNothing $ match @Maybe r []
matchesWholeM r as = any ((== 0) . length) $ match @Maybe r as

ones = Star (Lit (== 1))
bAfterA = Star (Star (Lit (/= 'b')) `Or` (Lit (== 'b') `Cat` Lit (== 'a')))
twos = Lit (== 2) `Or` Star (Lit (== 2))
