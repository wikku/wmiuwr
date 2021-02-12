open Set

module OrderedList (X : OrderedType) : (OrderedType with type t = X.t list) =
struct
  type t = X.t list
  let compare l1 l2 = match (l1, l2) with
    | (x :: xs, y :: ys) ->
        let c = X.compare x y in
        if c = 0 then compare xs ys else c
    | (x :: xs, []) -> 1
    | ([], y :: ys) -> -1
    | ([], []) -> 0
end

module OrderedPair (X : OrderedType) : (OrderedType with type t = X.t * X.t) =
struct
  type t = X.t * X.t
  let compare (x1, y1) (x2, y2) =
    let c = X.compare x1 x2 in
    if c = 0 then X.compare y1 y2 else c
end

module OrderedOpt (X : OrderedType) : (OrderedType with type t = X.t option) =
struct
  type t = X.t option
  let compare o1 o2 = match (o1, o2) with
    | (Some (x), Some (y)) -> X.compare x y
    | (None, Some(y)) -> -1
    | (Some (x), None) -> 1
    | (None, None) -> 0
end
