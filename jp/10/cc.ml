type void = |
type ('a, 'b) either = Left of 'a | Right of 'b

let abort : 'a. void -> 'a = function void -> .

module type cont = sig
  type 't cont
  val letcc : ('t cont -> 't) -> 't
  val throw : 't cont -> 't -> 's
end

module Zadanie4 (C : cont) = struct
  open C

  let callcc : type t s. ((t -> s) -> t) -> t =
    fun f -> letcc (fun k -> f (fun v -> throw k v))

  let c : type t. ((t -> void) -> void) -> t =
    fun f -> callcc (fun k -> abort (f (fun k' -> k k')))

  let em : type t. unit -> (t, t -> void) either =
    fun () -> callcc (fun k -> Right (fun t -> k (Left t)))

end
