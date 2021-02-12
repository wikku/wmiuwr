let is_generated : (module Perm.S with type t = 'a) -> 'a -> 'a list -> bool =
  fun (type a) (module S : Perm.S with type t = a) (p:a) perms ->
  let module Sp : (Set.S with type elt = a) = Set.Make(struct
      type t = S.t
      let compare = S.compare
  end) in
  let init = Sp.union (Sp.singleton (S.id)) (Sp.of_list perms) in
  let grow r s =
    let inv = Sp.singleton (S.invert r) in
    let shift = Sp.map (S.compose r) s in
    (Sp.union (Sp.union inv shift) s) in
  let succ s = Sp.fold grow s s in
  let rec iter prev =
    let rec next = succ prev in
    if Sp.equal next prev then next else iter next
  in Option.is_some (Sp.find_opt p (iter init))


module Pi = Perm.Make(Int)
let test1 () = is_generated (module Pi) (Pi.swap 2 3) [Pi.swap 1 2]
let test2 () = is_generated (module Pi) (Pi.swap 1 3) [Pi.swap 1 2; Pi.swap 1 3]
let test3 () = is_generated (module Pi) (Pi.id) []
let test4 () =
  let goal = (Pi.compose (Pi.swap 1 3) (Pi.swap 2 4)) in
  let perms = [Pi.swap 4 2; Pi.swap 3 1; Pi.swap 1 2] in
  is_generated (module Pi) goal perms


