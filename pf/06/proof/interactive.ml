open Proof
type status = None | Focused of goal | Unfocused of proof
let cur = ref None

let status () = !cur

let get_focused () = match !cur with | Focused (g) -> g | _ -> failwith ":("
let get_proof () = match !cur with | Unfocused (p) -> p | _ -> failwith ":("

let proof g f = cur := Focused (proof g f)
let goals () = goals (get_proof ())
let focus n = cur := Focused (focus n (get_proof ()))
let intro name = cur := Focused (intro name (get_focused ()))
let apply f = cur := Focused (apply f (get_focused()))
let apply_thm f = cur := Unfocused (apply_thm f (get_focused()))
let apply_assm f = cur := Unfocused (apply_assm f (get_focused()))
let intro_forall v = cur := Focused (intro_forall v (get_focused ()))
let generalize t = cur := Focused (generalize t (get_focused()))
let qed () = let res = qed (get_proof ()) in cur := None; res
