import Data.List
import Data.Maybe
import Control.Applicative

n `divBy` m = n `mod` m == 0
sqr x = x * x

f (x:xs) = [n | n <- xs, n `mod` x /= 0]

-- z1

primes = map head $ iterate f [2..]

-- z2

primes' = 2 : [p | p <- [3..]
                 , all (not . (p `divBy`)) $ takeWhile ((<= p) . sqr) primes']

-- z3

permi, perms :: [a] -> [[a]]

inserts :: a -> [a] -> [[a]]
inserts x [] = [[x]]
inserts x (y:ys) = (x:y:ys) : (map (y:) (inserts x ys))

permi [] = [[]]
permi (x:xs) = concatMap (inserts x) (permi xs)

selects [] = []
selects (x:xs) = (xs,x) : [(x:ys,y) | (ys,y) <- selects xs]

perms [] = [[]]
perms xs = [(y:zs) | (ys,y) <- selects xs, zs <- perms ys]

-- z4

sublist :: [a] -> [[a]]
sublist []     = [[]]
sublist (x:xs) = [r | sl <- sublist xs, r <- [sl, x:sl]]

-- z5

qsortBy :: (a -> a -> Bool) -> [a] -> [a]
qsortBy cmp [] = []
qsortBy cmp (x:xs) = qsortBy cmp smaller ++ [x] ++ qsortBy cmp greater
    where smaller = [y | y <- xs, y `cmp` x]
          greater = [y | y <- xs, not (y `cmp` x)]

-- z6

(><) :: [a] -> [b] -> [(a,b)]
{- działa tylko dla kwadratów
xs >< ys = concat (initDiags ++ tailDiags)
  where
    initDiags = map (zip xs . reverse) $ inits' ys
    tailDiags = [] -- zipWith (zipWith (,)) (tails' xs) (map reverse (tails' ys))
    tails' = tail . tails
    inits' = tail . inits
-}

xs >< ys = concat . takeWhile (not . null) $ diags
  where
    diags = map (catMaybes . zipWith (liftA2 (,)) xs') diagys
    diagys = map reverse . tail $ inits ys'
    xs' = map Just xs ++ repeat Nothing
    ys' = map Just ys ++ repeat Nothing
