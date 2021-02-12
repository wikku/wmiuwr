type var = char
type term = Free of var | Bound of int | Funsym of string * term list
type formula =
  | Falsum
  | Rel of string * term list
  | Implies of formula * formula
  | Forall of formula

let var v = Rel(String.make 1 v, [])

let equal_formula = (=)

let rec free_in_term c = function
  | Free(c') -> c' = c
  | Bound(_) -> false
  | Funsym(f, tl) -> List.exists (free_in_term c) tl

let rec free_in_formula v = function
  | Falsum -> false
  | Rel(r, tl) -> List.exists (free_in_term v) tl
  | Implies(a, c) -> free_in_formula v a || free_in_formula v c
  | Forall(f) -> free_in_formula v f

let rec subst_in_term t t0 = function
    | x when x = t -> t0
    | Funsym(f, tl) -> Funsym(f, List.map (subst_in_term t t0) tl)
    | x -> x

let rec has_bound = function
  | Bound(_) -> true
  | Funsym(f, tl) -> List.exists has_bound tl
  | Free(_) -> false

let rec subst v t =
  if has_bound t then failwith "can't subst" else function
    | Falsum -> Falsum
    | Rel(r, tl) -> Rel(r, List.map (subst_in_term (Free(v)) t) tl)
    | Implies(a, c) -> Implies(subst v t a, subst v t c)
    | Forall(f) -> Forall(subst v t f)

let rec subst_for_bound d t =
  if has_bound t then failwith "can't subst" else function
    | Falsum -> Falsum
    | Rel(r, tl) -> Rel(r, List.map (subst_in_term (Bound d) t) tl)
    | Implies(a, c) -> Implies(subst_for_bound d t a, subst_for_bound d t c)
    | Forall(f) -> Forall(subst_for_bound (d+1) t f)

let rec bind t d = function
  | Falsum -> Falsum
  | Rel(r, tl) -> Rel(r, List.map (subst_in_term t (Bound d)) tl)
  | Implies(a, c) -> Implies(bind t d a, bind t d c)
  | Forall(f) -> Forall(bind t (d+1) f)


let rec string_of_term = function
  | Free(c) -> String.make 1 c
  | Bound(n) -> string_of_int n
  | Funsym(t, l) ->
      t ^ (if l = [] then ""
               else "(" ^ String.concat ", " (List.map string_of_term l) ^ ")")

let string_of_formula f =
  let show_paren b s = if b then "(" ^ s ^ ")" else s in
  let rec aux p = function
    | Falsum -> "⊥"
    | Rel(p, l) ->
        p ^ (if l = [] then ""
                else "(" ^ String.concat ", " (List.map string_of_term l) ^ ")")
    | Implies(l, r) -> show_paren p (aux true l ^ " → " ^ aux false r)
    | Forall(f) -> "∀" ^ aux true f in
  aux false f

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

let neg f = Implies(f, Falsum)
let conj f1 f2 = neg (Implies (f1, neg f2))
let disj f1 f2 = Implies(Implies(f1, f2), f2)

module type Theory = sig
  type axiom
  val formula_of_axiom : axiom -> formula
end

module type L = sig

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

  val all_i : var -> theorem -> theorem
  val all_e : theorem -> term -> theorem

  type axiom
  val axiom : axiom -> theorem
end

module Make(Theory : Theory) : (L with type axiom = Theory.axiom) =
struct

  type theorem = formula list * formula

  let assumptions (a, c) = a

  let consequence (a, c) = c

  let pp_print_theorem fmtr thm =
    let open Format in
    pp_open_hvbox fmtr 2;
    begin match assumptions thm with
    | [] -> ()
    | f :: fs ->
      pp_print_formula fmtr f;
      fs |> List.iter (fun f ->
        pp_print_string fmtr ",";
        pp_print_space fmtr ();
        pp_print_formula fmtr f);
      pp_print_space fmtr ()
    end;
    pp_open_hbox fmtr ();
    pp_print_string fmtr "⊢";
    pp_print_space fmtr ();
    pp_print_formula fmtr (consequence thm);
    pp_close_box fmtr ();
    pp_close_box fmtr ()

  let by_assumption f = ([f], f)

  let imp_i f (a, c) =
    let a' = List.filter (fun x -> x <> f) a in
    (a', Implies(f, c))

  let imp_e (a1, c1) (a2, c2) =
    match c1 with
    | Implies (f1, f2) ->
        if f1 = c2 then
            (a1 @ a2, f2)
        else failwith "formulas don't match"
    | otherwise -> failwith "thm1 consequence has to be implication"

  let bot_e f (a, falsum) =
    if falsum = Falsum then (a, f)
    else failwith "thm has to prove false"

  let all_i v (a, c) =
    if List.exists (free_in_formula v) a then
        failwith "var free in assumptions"
    else (a, Forall(bind (Free v) 0 c))

  let all_e (a, c) t =
    match c with
    | Forall(f) -> (a, subst_for_bound 0 t f)
    | _ -> failwith "theorem has to prove forall"

  type axiom = Theory.axiom
  let axiom axm = ([], Theory.formula_of_axiom axm)

end
