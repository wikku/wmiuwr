type avl =
  | Empty
  | Node of
    { mutable l: avl;
      mutable v: int;
      mutable r: avl;
      mutable size: int;
      mutable height: int;
      mutable sum: int }

let height = function | Empty -> 0 | Node { height=h; _ } -> h
let size = function | Empty -> 0 | Node { size=s; _ } -> s
let sum = function | Empty -> 0 | Node { sum=s; _ } -> s

let delta = 1

let node_set l v r (out : avl) =
  let hl = height l and hr = height r in
  let h = if hl >= hr then hl + 1 else hr + 1 in
  (* if abs (hr - hl) > delta then failwith "imbalanced!" else *)
  match out with
  | Node o ->
    o.l <- l;
    o.v <- v;
    o.r <- r;
    o.size <- size l + 1 + size r;
    o.height <- h;
    o.sum <- sum l + v + sum r;
    out
  | Empty -> failwith "node_set out is Empty"

let rec sum i j acc = function
  | Empty -> acc
  | Node { l; v; r; size=si; sum=su; _ } ->
    if j <= 0 || i >= si then acc
    else if i <= 0 && j >= si then su + acc
    else let lsize = size l in
      let acc' = acc + (if i <= lsize && lsize < j then v else 0) in
      sum i j (sum (i - lsize - 1) (j - lsize - 1) acc' r) l

let bal l v r out =
  let hl = height l and hr = height r in
  if hl > hr + delta then match l with
    | Empty -> failwith "bal"
    | Node { l=ll; v=lv; r=lr; _ } ->
      if height ll >= height lr then
        node_set ll lv (node_set lr v r out) l
      else (match lr with
        | Empty -> failwith "bal"
        | Node { l=lrl; v=lrv; r=lrr; _ } ->
          node_set (node_set ll lv lrl l) lrv (node_set lrr v r out) lr)
  else if hr > hl + delta then match r with
    | Empty -> failwith "bal"
    | Node { l=rl; v=rv; r=rr; _ } ->
      if height rr >= height rl then
        node_set (node_set l v rl out) rv rr r
      else (match rl with
        | Empty -> failwith "bal"
        | Node { l=rll; v=rlv; r=rlr; _ } ->
          node_set (node_set l v rll out) rlv (node_set rlr rv rr r) rl)
  else node_set l v r out

(** insert before i-th (0-indexed) *)
let rec insert i x = function
  | Empty ->
    (* if i <> 0 then failwith "insert" else *)
    Node { l=Empty; v=x; r=Empty; size=1; height=1; sum=x };
  | Node { l; v; r; _ } as n ->
    let lsize = size l in
    if i <= lsize
    then bal (insert i x l) v r n
    else bal l v (insert (i - lsize - 1) x r) n

let rec remove i = function
  | Empty -> failwith "remove Empty"
  | Node { l; v; r; _ } as n ->
    let lsize = size l in
    let di = i - lsize in
    if di < 0 then
      let (nl, removed) = remove i l
      in (bal nl v r n, removed)
    else if di = 0 then
      (begin match (l, r) with
      | (Empty, r) -> r
      | (l, Empty) -> l
      | (l, r) -> let (nr, removed) = remove 0 r in bal l removed nr n
      end, v)
    else
      let (nr, removed) = remove (i - lsize - 1) r
      in (bal l v nr n, removed)

let rec list_of_avl = function
  | Empty -> []
  | Node { l; v; r; _ } -> list_of_avl l @ [v] @ list_of_avl r

let main () =
  (* Gc.set { (Gc.get()) with Gc.minor_heap_size = 1024 * 1024 * 16; max_overhead = -1 }; *)
  let open Scanf in
  scanf " %d " (fun n ->
  let arr = ref Empty in
  for iter = 1 to n do
    scanf " %c " (function
    | 'I' -> scanf " %d %d" (fun i x -> arr := insert i x !arr)
    | 'S' -> scanf " %d %d" (fun i j ->
      print_int (sum (i - 1) j 0 !arr); print_string "\n")
    | 'D' -> scanf " %d" (fun i -> arr := fst (remove (i - 1) !arr))
    | _ -> failwith "bad input")
  done)
  (* ; Gc.print_stat stderr *)

let _ = main ()
