import Data.List
import Test.QuickCheck

data Tree a = Node (Tree a) a (Tree a) | Leaf deriving Show

treeMember x Leaf = False
treeMember x (Node l n r) = case x `compare` n of
  LT -> treeMember x l
  EQ -> True
  GT -> treeMember x r

treeInsert x Leaf = Node Leaf x Leaf
treeInsert x t@(Node l n r) = case x `compare` n of
  LT -> Node (treeInsert x l) n r
  EQ -> t
  GT -> Node l n (treeInsert x r)

treeDelete x Leaf = Leaf
treeDelete x t@(Node l n r) = case x `compare` n of
  LT -> Node (treeDelete x l) n r
  GT -> Node l n (treeDelete x r)
  EQ -> case (l,r) of
          (Leaf, _) -> r
          (_, Leaf) -> l
          _ -> Node l minr r'
            where minr = treeMin r
                  r' = treeDelete minr r
                  treeMin (Node Leaf n _) = n
                  treeMin (Node l _ _) = treeMin l

foldTree f z Leaf = z
foldTree f z (Node l n r) = foldTree f (f n (foldTree f z r)) l

t1 `treeSubtract` t2 = foldTree treeDelete t1 t2
t1 `treeUnion` t2 = foldTree treeInsert t1 t2

treeFromList xs = foldr treeInsert Leaf xs
treeToList xs = foldTree (:) [] xs

data Set a = Fin (Tree a) | Cofin (Tree a)

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
