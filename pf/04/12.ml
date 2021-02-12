let range n = List.init n (fun n -> n)

(* z1 *)
type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree

let balanced t =
  let exception Unbalanced in
  let rec go = function
    | Leaf -> 0
    | Node (l, _, r) ->
        let (ls, rs) = (go l, go r) in
        if abs(ls - rs) > 1 then raise Unbalanced else ls + 1 + rs
  in
  try Fun.const true (go t) with Unbalanced -> false

let rec split_at = function
  | (_, []) -> ([], [])
  | (0, xs) -> ([], xs)
  | (n, x :: xs) -> let (ys, zs) = split_at (n-1, xs) in (x :: ys, zs)

let halve lst = split_at ((List.length lst)/2, lst)

let rec build = function
  | [] -> Leaf
  | x :: xs -> let (l, r) = halve xs in Node (build l, x, build r)

(* z2 *)
type 'a place = Place of 'a list * 'a list
(* odwrócona lista na lewo i lista na prawo od miejsca *)

let findNth lst n =
  let rec aux revleft right n = match (right, n) with
    | ([], 0) -> Place (revleft, [])
    | ([], _) -> failwith "Out of bounds"
    | (lst, 0) -> Place (revleft, right)
    | (x :: xs, n) -> aux (x :: revleft) xs (n-1)
  in
  aux [] lst n

let collapse (Place (l, r)) = List.rev_append l r

let add a (Place (l, r)) = Place (l, a :: r)
let del (Place (l, r :: rs)) = Place (l, rs)

let next (Place (ls, r :: rs)) = Place (r :: ls, rs)
let prev (Place (l :: ls, rs)) = Place (ls, l :: rs)

type side = Left | Right
type 'a btplace = (side * 'a * 'a btree) list * 'a btree
(* miejsce w drzewie to ścieżka do korzenia (w którym poddrzewie jesteśmy, jaki
 * element jest wyżej i jakie drzewo jest obok) i poddrzewo *)
