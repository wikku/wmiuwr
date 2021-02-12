let id x = x

(* z1 *)
let length lst = List.fold_left (fun n _ -> n+1) 0 lst
let rev lst = List.fold_left (Fun.flip List.cons) [] lst
let map f lst = List.fold_right (fun a l -> f a :: l) lst [];;
let append l1 l2 = List.fold_right List.cons l1 l2
let rev_append l1 l2 = List.fold_left (Fun.flip List.cons) l2 l1;;
let filter pred lst =
  List.fold_right (fun a l -> if pred a then a :: l else l) lst [];;
let rev_map f lst = List.fold_left (fun l a -> f a :: l) [] lst;;

(* z2 *)
let parse_decimal lst =
  let rec go lst acc = match lst with
  | [] -> acc
  | x :: xs -> go xs (10*acc + x)
  in go lst 0

let parse_decimal' lst = List.fold_left (fun n d -> 10*n + d) 0 lst;;

(* z3 *)
let poly3a lst x =
  let rec go lst acc = match lst with
  | [] -> acc
  | c :: cs -> go cs (x *. acc +. c)
  in go lst 0.0

let poly3b lst x = List.fold_left (fun acc c -> x*.acc +. c) 0.0 lst;;

(* z4 *)
let rec poly4a lst x = match lst with
  | [] -> 0.0
  | c :: cs -> x *. (poly4a cs x) +. c

let poly4b lst x = List.fold_right (fun c y -> x *. y +. c) lst 0.0

let poly4c lst x =
  let rec go lst xpow acc = match lst with
  | [] -> acc
  | c :: cs -> go cs (xpow *. x) (acc +. xpow *. c)
  in go lst 1.0 0.0

let poly4d lst x =
  let f (xpow, acc) c = (xpow *. x, acc +. xpow *. c) in
  snd (List.fold_left f (1.0, 0.0) lst);;

(* z5 *)
let for_all pred lst =
  let exception Short_circuit in
  let raising_pred b a = if pred a then b else raise Short_circuit in
  try List.fold_left raising_pred true lst with Short_circuit -> false

let mult_list lst =
  let exception Mult_by_zero in
  let mul p i = if i <> 0 then p * i else raise Mult_by_zero in
  try List.fold_left mul 1 lst with Mult_by_zero -> 0


let sorted = function
| [] -> true
| x :: xs ->
  let exception Nonincreasing in
  let cmp a b = if a < b then b else raise Nonincreasing in
  try
    Fun.const true (List.fold_left cmp x xs)
  with Nonincreasing -> false

(* z6 *)
let rec fold_left_cps f z lst k =
  match lst with
  | [] -> k z
  | x :: xs -> f z x (fun a -> fold_left_cps f a xs k)

let fold_left f a lst = fold_left_cps (fun a b k -> k (f a b)) a lst id

(* z7 *)
let for_all' pred lst =
  fold_left_cps (fun _ a k -> if pred a then (k true) else false) true lst id

let mult_list' lst =
  fold_left_cps (fun p i k -> if i <> 0 then k (p * i) else 0) 1 lst id

let sorted' = function
| [] -> true
| x :: xs ->
  let f (a, _) b k = if a < b then k (b, true) else (42, false) in
  snd (fold_left_cps f (x, true) xs id)
