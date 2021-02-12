module Fmt where
import Prelude hiding ((^^))

lit :: String -> (String -> a) -> String -> a
lit l k s = k (s ++ l)

int :: (String -> a) -> String -> Integer -> a
int k s i = k (s ++ show i)

str :: (String -> a) -> String -> String -> a
str k s s' = k (s ++ s')

sprintf k = k id ""

(^^) = (.)

test1 = sprintf $ lit "Ala ma " ^^ int ^^ lit " kot" ^^ str ^^ lit "."
