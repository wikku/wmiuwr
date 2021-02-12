(* z7 *)
type 'a comp = Ready of 'a | Thunk of (unit -> 'a) | Blackhole
type 'a my_lazy = { mutable l : 'a comp }

let force ml = match ml.l with
  | Ready (x) -> x
  | Thunk (t) -> ml.l <- Blackhole;
                 let v = t () in ml.l <- Ready v;
                 v
  | Blackhole -> failwith "disallowed recursion"

let rec fix f =
  let rec res = { l=Thunk (fun () -> f res) } in
  res

let my_lazy t = { l=Thunk (t) }

type 'a stream = Empty | Cons of 'a * 'a stream my_lazy
let hd = function | Cons (x, _) -> x | _ -> failwith "empty"
let tl = function | Cons (_, x) -> force x | _ -> failwith "empty"

let rec map f s = my_lazy (fun () -> match force s with
  | Empty -> Empty
  | Cons (hd, tl) -> Cons (f hd, map f tl))


let rec filter f s = my_lazy (fun () -> match force s with
  | Cons (hd, tl) ->
      if f hd then Cons (hd, filter f tl)
      else force (filter f tl)
  | Empty -> Empty)



let rec sieve s = my_lazy (fun () -> match force s with
  | Cons (p, tl) -> Cons (p, sieve (filter (fun n -> n mod p <> 0) tl))
  | _ -> failwith ":(")

let rec stream_from n = my_lazy (fun () -> Cons (n, stream_from (n+1)))

let primes = sieve (stream_from 2)
