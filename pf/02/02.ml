let range n = List.init n (fun x -> x)

(* z1 *)

let rec map_rev_append f xs ys = match xs with
  | [] -> ys
  | x :: xs -> map_rev_append f xs (f x :: ys)

let rec sublists = function
    (x :: xs) -> let sxs = sublists xs in
                   map_rev_append (fun lst -> (x :: lst)) sxs sxs
  | []        -> [[]]

(* z2 *)
let rec split_at = function
    (_, [])        -> ([], [])
  | (0, xs)        -> ([], xs)
  | (n, (x :: xs)) -> let (ys, zs) = split_at (n-1, xs) in (x :: ys, zs)

let cycle lst n = let (init, tail) = split_at (List.length lst - n, lst) in
                    tail @ init

(* z3.1 *)
let rec merge cmp lst1 lst2 = match (lst1, lst2) with
    ([], l) -> l
  | (l, []) -> l
  | (x :: xs as xxs), (y :: ys as yys) -> if cmp x y
        then x :: merge cmp xs yys
        else y :: merge cmp xxs ys
(* z3.2 *)

let merge' cmp lst1 lst2 =
    let rec go lst1 lst2 acc = match (lst1, lst2) with
        ([], l) | (l, []) -> List.rev_append l acc
      | (x :: xs as xxs), (y :: ys as yys) -> if cmp x y
            then go xs yys (x :: acc)
            else go xxs ys (y :: acc)
    in go lst1 lst2 [] |> List.rev

(* benchmarkowanie *)
let time proc =
    let start = Unix.gettimeofday () in
    let res = proc () in
    let stop = Unix.gettimeofday () in
    stop -. start

let bench_sorted merge =
    time (fun _ -> merge (<=) (range 10000) (range 10000))
(* w interpreterze merge wydaje się lekko szybszy *)

(* z3.3 *)
let rec halve = function
    [] -> ([], [])
  | x :: [] -> ([], [x])
  | x :: y :: ys -> let (even, odd) = halve ys in (x :: even, y :: odd)

(* z3.4 *)
let rec mergesort cmp = function
    [] -> []
  | x :: [] -> [x]
  | xs -> let (even, odd) = halve xs in
          merge cmp (mergesort cmp even) (mergesort cmp odd)

(* z3.5 *)
let merge'' cmp lst1 lst2 =
    let rec go lst1 lst2 acc = match (lst1, lst2) with
        ([], l) | (l, []) -> List.rev_append l acc
      | (x :: xs as xxs), (y :: ys as yys) -> if cmp x y
            then go xs yys (x :: acc)
            else go xxs ys (y :: acc)
    in go lst1 lst2 []

let rec mergesort'' cmp =
    (* może lepiej byłoby przekazywać jakieś type cmp = | Asc | Desc zamiast
     * do log_2 n zagnieżdżonych lambd *)
    let notcmp = fun x y -> not (cmp x y) in function
        [] -> []
      | x :: [] -> [x]
      | xs -> let (even, odd) = halve xs in
                merge'' notcmp (mergesort'' notcmp even) (mergesort'' notcmp odd)


(* z4 *)

let rotations xs = List.map (fun n -> cycle xs n) (range (List.length xs))

let rec permutations = function
    []      -> [[]]
  | x :: xs -> List.concat_map (fun lst -> rotations (x::lst)) (permutations xs)

(* z5 *)
let rec suffixes = function
    [] -> [[]]
  | x :: xs as xxs -> xxs :: suffixes xs

let prefixes lst = List.rev (List.map List.rev (suffixes (List.rev lst)))

(* z6 *)

type 'a clist = { clist : 'z. ('a -> 'z -> 'z) -> 'z -> 'z }
let cnil : 'a clist = { clist = fun f z -> z }
let ccons : 'a -> 'a clist -> 'a clist =
    fun a c -> { clist = fun f z -> f a (c.clist f z) }
let map : ('a -> 'b) -> 'a clist -> 'b clist =
    fun g c -> { clist = fun f z ->
                            let fg = fun x -> f (g x) in
                            (c.clist fg z) }
let append : 'a clist -> 'a clist -> 'a clist =
    fun c1 c2 -> { clist = fun f z -> c1.clist f (c2.clist f z) }
let clist_to_list : 'a clist -> 'a list =
    fun c -> c.clist (fun car cdr -> car :: cdr) []
let clist_of_list : 'a list -> 'a clist =
    fun lst -> { clist = fun f z -> List.fold_right f lst z }

(* iloczyn kartezjański *)
let prod : 'a clist -> 'b clist -> ('a * 'b) clist =
    fun ca cb -> { clist =
        fun f z -> ca.clist (fun a -> cb.clist (fun b -> f (a,b))) z }


(* poprzez analogię z teorią zbiorów można by chcieć mieć
 *     pow : 'a clist -> 'b clist -> ('b -> 'a) clist
 * ale listy to nie zbiory: w 'b clist niekoniecznie są wszystkie możliwe 'b
 * oraz elementy mogą sie powtarzać, więc byłoby trudno zdefiniować sensowne
 * funkcje 'b -> 'a, nawet częściowe *)
(* dlatego traktuję 'b clist po prostu jako liczbę, zapominając o elementach *)

let concat_map : ('a -> 'b clist) -> 'a clist -> 'b clist =
    fun f ca -> (map f ca).clist append cnil
let pow : 'a clist -> 'b clist -> 'a clist clist =
    fun ca cb ->
        cb.clist
            (fun _ cca -> concat_map (fun a -> map (ccons a) cca) ca)
            (ccons cnil cnil)

(* z7 *)
type cnat = { cnat : 'a. ('a -> 'a) -> 'a -> 'a }
let zero : cnat = { cnat = fun f z -> z }
let succ : cnat -> cnat = fun cn -> { cnat = fun f z -> f (cn.cnat f z) }
let int_of_cnum : cnat -> int = fun cn -> cn.cnat ((+) 1) 0
let pred : cnat -> cnat = fun cn ->
    fst (cn.cnat (fun (fst, snd) -> (snd, succ snd)) (zero, zero))

let ctail : 'a clist -> 'a clist = fun c ->
    fst (c.clist (fun a (fst, snd) -> (snd, ccons a snd)) (cnil, cnil))

let () = Printf.printf "xd"
