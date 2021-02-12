(** reprezentacja formuł *)

type var = char
type term = Free of var | Bound of int | Funsym of string * term list
type formula =
  | Falsum
  | Rel of string * term list
  | Implies of formula * formula
  | Forall of formula

val var : char -> formula

val pp_print_formula : Format.formatter -> formula -> unit

val equal_formula : formula -> formula -> bool

(** reprezentacja twierdzeń *)
type theorem

(** założenia twierdzenia *)
val assumptions : theorem -> formula list
(** teza twierdzeni *)
val consequence : theorem -> formula

val pp_print_theorem : Format.formatter -> theorem -> unit

(** by_assumption f konstuuje następujący dowód

  -------(Ax)
  {f} ⊢ f  *)
val by_assumption : formula -> theorem

(** imp_i f thm konstuuje następujący dowód

       thm
      Γ ⊢ φ
 ---------------(→I)
 Γ \ {f} ⊢ f → φ *)
val imp_i : formula -> theorem -> theorem

(** imp_e thm1 thm2 konstuuje następujący dowód

    thm1      thm2
 Γ ⊢ φ → ψ    Δ ⊢ φ 
 ------------------(→E)
 Γ ∪ Δ ⊢ ψ *)
val imp_e : theorem -> theorem -> theorem

(** bot_e f thm konstruuje następujący dowód

   thm
  Γ ⊢ ⊥
  -----(⊥E)
  Γ ⊢ f *)
val bot_e : formula -> theorem -> theorem

val free_in_term : var -> term -> bool
val free_in_formula : var -> formula -> bool
val subst : var -> term -> formula -> formula
val subst_for_bound : int -> term -> formula -> formula
val bind : term -> int -> formula -> formula

val all_i : var -> theorem -> theorem
val all_e : theorem -> term -> theorem

val neg : formula -> formula
val conj : formula -> formula -> formula
val disj : formula -> formula -> formula
