let id x = x

open Proc

let rec map : ('i -> 'o) -> ('a, 'z, 'i, 'o) proc = fun f k ->
  recv (fun v ->
  send (f v) (fun () ->
  map f k))

let rec filter : ('i -> bool) -> ('a, 'z, 'i, 'i) proc = fun p k ->
  let end_cont = fun () -> filter p k in
  recv (fun v ->
  if p v then send v end_cont else end_cont ())

let rec nats_from : int -> ('a, 'z, 'i, int) proc = fun n k ->
  send n (fun () -> nats_from (n+1) k)

let rec sieve : ('a, 'a, int, int) proc = fun k ->
  recv (fun n ->
  send n (fun () ->
  (filter (fun m -> m mod n <> 0) <|>> sieve) k))
