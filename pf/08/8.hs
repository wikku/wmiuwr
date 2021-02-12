import Data.List
--import Test.QuickCheck

data Color = Red | Black
  deriving (Show)
data RBTree a = Empty | Node Color a (RBTree a) (RBTree a)
  deriving (Show)
treeFromList xs = foldr treeInsert Leaf xs
treeToList xs = foldTree (:) [] xs


data Set a = Fin (RBTree a) | Cofin (RBTree a)

setEmpty, setFull :: Ord a => Set a
setEmpty = Fin Leaf
setFull = Cofin Leaf

setComplement :: Ord a => Set a -> Set a
setComplement (Fin t) = Cofin t
setComplement (Cofin t) = Fin t

setUnion, setIntersection :: Ord a => Set a -> Set a -> Set a
setUnion (Fin x) (Fin y) = Fin (treeUnion x y)
setUnion (Cofin x) (Cofin y) = Cofin (x `treeSubtract` (x `treeSubtract` y))
setUnion (Cofin x) (Fin y) = Cofin (x `treeSubtract` y)
setUnion (Fin x) (Cofin y) = setUnion (Cofin y) (Fin x)

setIntersection x y =
  setComplement $ setUnion (setComplement x) (setComplement y)

setMember :: Ord a => a -> Set a -> Bool
setMember x (Fin t) = treeMember x t
setMember x (Cofin t) = not $ treeMember x t

setFromList xs = Fin (treeFromList xs)


-- quickcheck
{-
prop_fromToList xs = foldTree (:) [] (treeFromList xs) == (nub . sort $ xs)
  where types = xs :: [Int]
prop_treeUnion xs ys = treeToList (treeFromList xs `treeUnion` treeFromList ys) == nub (sort (xs++ys))
  where types = xs :: [Int]

prop_RevRev xs = reverse (reverse xs ++ xs) == xs
-}
