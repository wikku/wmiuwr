module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type key
  type t
  val apply   : t -> key -> key
  val id      : t
  val invert  : t -> t
  val swap    : key -> key -> t
  val compose : t -> t -> t
  val compare : t -> t -> int
end

module Make(Key : Map.OrderedType) : (S with type key = Key.t) = struct
  module M = Map.Make(Key)
  type key = M.key
  type t = { forward : key M.t; backward : key M.t }
  let apply { forward=forward; } k = match M.find_opt k forward with
    | None -> k
    | Some(v) -> v
  let id = { forward=M.empty; backward=M.empty }
  let invert { forward; backward } = { forward=backward; backward=forward }
  let swap k1 k2 =
    let m = M.add k2 k1 (M.singleton k1 k2) in { forward=m; backward=m }
  let compose { forward=fw1; backward=bw1 } { forward=fw2; backward=bw2 } =
    let f m2 k y z = match (y,z) with
      | (Some (y), None) -> Some(y)
      | (None, Some(z)) -> Some(z)
      | (Some(y), Some(z)) -> M.find_opt y m2
      | _ -> None
    in
    { forward=M.merge (f fw2) fw1 fw2; backward=M.merge (f bw1) bw2 bw1 }
  let inv_key_compare a b = Key.compare b a
  let compare { forward=fw1; backward=bw1 } { forward=fw2; backward=bw2 } =
    M.compare Key.compare fw1 fw2
end


