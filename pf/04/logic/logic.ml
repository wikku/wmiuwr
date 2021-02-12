type formula = Falsum | Var of char | Implies of formula * formula

let string_of_formula f =
  let show_paren b s = if b then "(" ^ s ^ ")" else s in
  let rec aux p = function
    | Falsum -> "⊥"
    | Var(c) -> String.make 1 c
    | Implies(l, r) -> show_paren p (aux true l ^ " → " ^ aux false r) in
  aux false f

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

type judgement = Jdgt of formula list * formula
(* a judgement is a consequence following from antecedents *)
type theorem = Drv of theorem list * judgement
(* a theorem is a judgement with a proof derived from other theorems *)

let assumptions (Drv (_, Jdgt (a, _))) = a

let consequence (Drv (_, Jdgt (_, c))) = c

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

let by_assumption f =
  Drv ([], Jdgt ([f], f))

let imp_i f (Drv (_, Jdgt(a, c)) as thm) =
  let a' = List.filter (fun x -> x <> f) a in
  Drv ([thm], Jdgt (a', Implies(f, c)))

let imp_e (Drv (_, Jdgt (a1, c1)) as thm1) (Drv (_, Jdgt (a2, c2)) as thm2) =
  match c1 with
  | Implies (f1, f2) ->
      if f1 = c2 then
          Drv ([thm1; thm2], Jdgt (a1 @ a2, f2))
      else failwith "formulas don't match"
  | otherwise -> failwith "thm1 consequence has to be implication"

let bot_e f (Drv (_, Jdgt (a, falsum)) as thm) =
  if falsum = Falsum then Drv ([thm], Jdgt (a, f))
  else failwith "thm has to prove false"
