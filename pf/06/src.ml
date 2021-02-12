(* z1 *)
open Lazy

let rec double_f double n =
  if n == 0 then 0
  else 2 + double (n-1)

let fib_f fib n =
  if n <= 1 then n
  else fib (n-1) + fib (n-2)

let rec fix f x = f (fix f) x

let rec fix_with_limit n f x =
  if n = 0
  then failwith "recursion limit exceeded"
  else f (fix_with_limit (n-1) f) x

let fix_memo f =
  let memo = Hashtbl.create 1 in
  let rec aux x = match Hashtbl.find_opt memo x with
    | Some (y) -> y
    | None -> let y = f aux x in Hashtbl.add memo x y; y
  in
  aux

(* z2 *)

(* let fix f g = (fun x a -> f (x x) a) (fun x a -> f (x x) a) g
                                  ^
This expression has type 'a -> 'b but an expression was expected of type 'a *)
type 'a mu = { roll : 'a mu -> 'a }
let unroll { roll=x } = x
let fix_rt f =
  (fun x a -> f (unroll x x) a) { roll = fun x a -> f (unroll x x) a }

let fix_magic f : 'a -> 'a =
  (fun x a -> f (Obj.magic x x) a) (fun x a -> f (Obj.magic x x) a)


(* z3 *)

let (next, reset) =
  let cnt = ref 0 in
  let next () = let r = !cnt in cnt := r + 1; r in
  let reset () = cnt := 0 in
  (next, reset)

(* z4 *)
type 'a stream = Empty | Cons of 'a * 'a stream lazy_t

let stream_from f =
  let rec aux n = match f n with
    | Some(a) -> Cons (a, lazy (aux (n+1)))
    | None -> Empty
  in aux 0

let scan_left f z =
  let rec aux acc s = Cons (acc, match s with
    | Empty -> lazy Empty
    | Cons (a, lazy tl) -> lazy ( aux (f acc a) tl))
  in
  aux z

let rec take n s =
  if n = 0 then [] else match s with
  | Empty -> []
  | Cons (a, lazy tl) -> a :: take (n-1) tl

let terms = stream_from (fun n ->
  Some ((if (n mod 2) = 0 then 4.0 else -4.0) /. (float_of_int (2*n+1))))
let partial_sums = scan_left (+.) 0.0 terms

let rec map_triples f = function
  | Cons (x, (lazy (Cons (y, lazy (Cons (z, _))) as tl))) ->
      Cons (f x y z, lazy (map_triples f tl))
  | _ -> Empty

let pi_approx =
  let f x y z = z -. ((y -. z)*.(y -. z))/.(x -. 2.0*.y +. z) in
  map_triples f partial_sums

(* z5 *)
type 'a dllist = 'a dllist_data lazy_t
and 'a dllist_data =
  { prev : 'a dllist
  ; elem : 'a
  ; next : 'a dllist
  }

let prev (lazy { prev; elem; next }) = prev
let elem (lazy { prev; elem; next }) = elem
let next (lazy { prev; elem; next }) = next
(*
let of_list = function
  | [] -> failwith "cannot be empty"
  | x :: xs ->

          *)
(*
let of_list = function
  | [] -> failwith "argument empty"
  | x :: xs ->
      let rec hd = (lazy { prev=hd; elem=x; next=hd }) in
      let rec step (lazy { prev; elem; next }) x =
        let rec added =
          (lazy { prev=(lazy { prev=prev
                             ; elem=elem
                             ; next=added })
                ; elem=x
                ; next=(Lazy.force result) }) in
        added
      and result = lazy (List.fold_left step hd xs)
      in Lazy.force (result)
*)
type 'a ldllist = 'a dllist lazy_t

let of_list : 'a list -> 'a dllist = function
  | [] -> failwith "empty"
  | x :: xs ->
      let rec result = lazy { prev=last; elem=x; next=next }
      and tied = lazy (tie result xs)
      and next = lazy (force (fst (force tied)))
      and last = lazy (force (snd (force tied)))
      and tie p = function
        | [] -> (result, p)
        | hd :: tl -> let rec this = lazy { elem=hd; prev=p; next=n }
                      and nb = lazy (tie this tl)
                      and n = lazy (force (fst (force nb)))
                      and l = lazy (force (snd (force nb)))
                      in
                      (this, l)
      in result
