open Logic

type context = (string * formula) list
type goalDesc = context * formula

type rule = Assm | ImpI | ImpE | BotE | AllI of var | AllE of term

type proof =
  | Proven of theorem
  | Goal of goalDesc
  | Derived of proof list * int * goalDesc * rule


let qed pf =
  match pf with
  | Proven thm -> thm
  | _ -> failwith "proof unfinished"

let rec numGoals pf =
  match pf with
  | Proven _ -> 0
  | Goal _ -> 1
  | Derived (_, n, _, _) -> n

let sumNumGoals = List.fold_left (fun n p -> n + numGoals p) 0

let apply_rule rule thms (ctx, f) =
  match rule with
  | Assm -> by_assumption f
  | ImpI -> (match f with
       | Implies(p,q) -> imp_i p (List.hd thms)
       | _ -> failwith "internal error")
  | ImpE -> (match thms with
       | [thm1; thm2] -> imp_e thm1 thm2
       | _ -> failwith "internal error")
  | BotE -> bot_e f (List.hd thms)
  | AllI(c) -> all_i c (List.hd thms)
  | AllE(t) -> all_e (List.hd thms) t

let derive pfs goal rule =
  let n = sumNumGoals pfs in
  if n = 0 then
      let extract = (function Proven t -> t | _ -> failwith "internal error") in
      Proven (apply_rule rule (List.map extract pfs) goal)
  else Derived (pfs, n, goal, rule)


let rec goals pf =
  match pf with
  | Proven _ -> []
  | Goal g -> [g]
  | Derived (lst, _, _, _) -> List.concat (List.map goals lst)

let proof g f = ([], (g, f))

type goal = (proof list * goalDesc * proof list * rule) list * goalDesc

let goal (_, desc) = desc

let find n pfs =
  let rec aux m acc = function
  | [] -> failwith "wrong n"
  | p :: ps ->
      let pn = numGoals p in
      if m+pn <= n then aux (m+pn) (p::acc) ps
      else (acc, p, ps, m)
  in aux 0 [] pfs

let focus n pf =
  let rec aux acc n = function
  | Goal g -> if n = 0 then (acc, g) else failwith "wrong n"
  | Proven _ -> failwith "wrong n"
  | Derived (pfs, _, desc, rule) ->
      let (l, p, r, ln) = find n pfs in
      aux ((l, desc, r, rule) :: acc) (n-ln) p
  in
  aux [] n pf

let fc = focus 0

let rec rebuild_path pf = function
  | [] -> pf
  | (l, d, r, rule) :: ps ->
      rebuild_path (derive (List.rev_append l (pf :: r)) d rule) ps

let unfocus (path, desc) = rebuild_path (Goal desc) path

let next (path, desc) =
  let rec aux lp = function
    | [] -> focus 0 lp
    | (l, d, r, rule) :: ps ->
        let subtree = derive (List.rev_append l (lp :: r)) d rule in
        if List.exists (fun p -> numGoals p > 0) r then
            let ls = sumNumGoals l in
            let (sl, sd) = focus (ls + numGoals lp) subtree in
            (sl @ ps, sd)
        else aux subtree ps
  in aux (Goal desc) path

let prev (path, desc) =
  let rec aux rp = function
    | [] -> focus (numGoals rp - 1) rp
    | (l, d, r, rule) :: ps ->
        let subtree = derive (List.rev_append l (rp :: r)) d rule in
        if List.exists (fun p -> numGoals p > 0) l then
            let ls = sumNumGoals l in
            let (sl, sd) = focus (ls - 1) subtree in
            (sl @ ps, sd)
        else aux subtree ps
  in aux (Goal desc) path

let intro name (path, ((assm, f) as g)) =
  match f with
  | Implies (p, q) -> (([], g, [], ImpI) :: path, ((name, p) :: assm, q))
  | _ -> failwith "goal is not implication"

let apply f gl =
  let rec aux f ((path, (assm, fi)) as gl) =
    match f with
    | f when fi = f -> gl
    | Implies (p, q) ->
        let (path, (_, f)) = aux q gl in
        (([], (assm, f), [Goal(assm, p)], ImpE) :: path, (assm, Implies(p, q)))
    | Falsum -> (([], (assm, fi), [], BotE) :: path, (assm, Falsum))
    | _ -> failwith "wrong f"
  in aux f gl

let apply_thm thm (_, (ctx, _) as gl) =
  let gl_assms = List.map snd ctx in
  if List.for_all (fun f -> List.exists ((=) f) gl_assms) (assumptions thm) then
      let (path, _) = apply (consequence thm) gl in
      rebuild_path (Proven thm) path
  else failwith "wrong assumptions"

let apply_assm name (_, (ctx, _) as gl) =
  let assm = List.assoc name ctx in
  apply_thm (by_assumption assm) gl

let pp_print_proof fmtr pf =
  let ngoals = numGoals pf
  and goals = goals pf
  in if ngoals = 0
  then Format.pp_print_string fmtr "No more subgoals"
  else begin
      Format.pp_open_vbox fmtr (-100);
      Format.pp_open_hbox fmtr ();
      Format.pp_print_string fmtr "There are";
      Format.pp_print_space fmtr ();
      Format.pp_print_int fmtr ngoals;
      Format.pp_print_space fmtr ();
      Format.pp_print_string fmtr "subgoals:";
      Format.pp_close_box fmtr ();
      Format.pp_print_cut fmtr ();
      goals |> List.iteri (fun n (_, f) ->
       Format.pp_print_cut fmtr ();
       Format.pp_open_hbox fmtr ();
       Format.pp_print_int fmtr (n + 1);
       Format.pp_print_string fmtr ":";
       Format.pp_print_space fmtr ();
       pp_print_formula fmtr f;
       Format.pp_close_box fmtr ());
      Format.pp_close_box fmtr ()
    end

let pp_print_goal fmtr gl =
  let (g, f) = goal gl
  in
  Format.pp_open_vbox fmtr (-100);
  g |> List.iter (fun (name, f) ->
      Format.pp_print_cut fmtr ();
      Format.pp_open_hbox fmtr ();
      Format.pp_print_string fmtr name;
      Format.pp_print_string fmtr ":";
      Format.pp_print_space fmtr ();
      pp_print_formula fmtr f;
      Format.pp_close_box fmtr ());
  Format.pp_print_cut fmtr ();
  Format.pp_print_string fmtr (String.make 40 '=');
  Format.pp_print_cut fmtr ();
  pp_print_formula fmtr f;
  Format.pp_close_box fmtr ()

let p = Rel("p",[])
and q = Rel("q",[])
and r = Rel("r",[])
let z6f1 = Implies (p, Implies(Implies (p,q), q))
let z6p1 = proof [] z6f1 |> intro "H1" |> intro "H2"
    |> apply_assm "H2" |> focus 0 |> apply_assm "H1" |> qed

let z6f2 = Implies (Implies (p, Implies(q,r)), Implies (Implies (p,q), Implies(p,r)))
let z6p2 = proof [] z6f2 |> intro "pqr" |> intro "pq" |> intro "p"
    |> apply_assm "pqr" |> focus 0 |> apply_assm "p" |> focus 0 |> apply_assm
    "pq" |> focus 0 |> apply_assm "p" |> qed

let z6f3 = Implies(Implies (Implies(Implies(p, Falsum), p), p),
                   Implies(Implies(Implies(p, Falsum), Falsum),p))
let z6p3 = proof [] z6f3 |> intro "pfpp" |> intro "pff"
    |> apply_assm "pfpp" |> focus 0 |> intro "pf" |> apply_assm "pfpp"
    |> focus 0 |> apply_assm "pff" |> focus 0 |> apply_assm "pf" |> qed

let z6f4 = Implies(Implies (Implies(Implies(p, Falsum), Falsum), p),
                   Implies(Implies(Implies(p, Falsum), p),p))

let z6p4 = proof [] z6f4 |> intro "pffp" |> intro "pfp"
    |> apply_assm "pffp" |> fc |> intro "pf" |> apply_assm "pf" |> fc
    |> apply_assm "pfp" |> fc |> apply_assm "pf" |> qed

let z6p5 = Goal (
  [
    ("pf", Implies(p, Falsum));
  ], Implies(p, Falsum))


let intro_forall v (path, ((assm, f) as g)) =
  if List.exists (free_in_formula v) (List.map snd assm)
  then failwith "var free in assumptions"
  else match f with
  | Forall(f') ->
      (([], g, [], AllI(v)) :: path, (assm, subst_for_bound 0 (Free v) f'))
  | _ -> failwith "goal is not forall"

let generalize t (path, ((assm, f) as g)) =
  (([], g, [], AllE(t)) :: path, (assm, Forall(bind t 0 f)))

let fof1 =
    Implies(Forall(Forall(Rel("p", [Bound 0; Bound 1]))),
            Forall(Forall(Rel("p", [Bound 1; Bound 0]))))
let fop1 = proof [] fof1 |> intro "L" |> intro_forall 'x' |> intro_forall 'y'
  |> generalize (Free 'x') |> generalize (Free 'y') |> apply_assm "L" |> qed

let fof2 =
  Implies(conj (Forall (Rel("P", [Bound 0]))) (Forall (Rel("Q", [Bound 0]))),
          Forall (conj (Rel("P", [Bound 0])) (Rel("P", [Bound 0]))))
let fop2 =
  proof [] fof2 |> intro "L" |> intro_forall 'x' |> intro "A1"
  |> apply_assm "L" |> fc |> intro "A2" |> intro "A3" |> apply_assm "A1"
  |> fc |> generalize (Free 'x') |> apply_assm "A2" |> fc
  |> generalize (Free 'x') |> apply_assm "A2" |> qed

let fof3 = Implies(conj (Rel("P",[])) (Forall(Rel("Q",[Bound 0]))),
                   Forall(conj (Rel("P",[])) (Rel("Q",[Bound 0]))))

let fop3 =
  proof [] fof3 |> intro "L" |> intro_forall 'x' |> intro "A1"
  |> apply_assm "L" |> fc |> intro "A2" |> intro "A3" |> apply_assm "A1"
  |> fc |> apply_assm "A2" |> fc |> generalize (Free 'x') |> apply_assm "A3"
  |> qed

let fof4 = Implies(disj (Rel("P",[])) (Forall(Rel("Q",[Bound 0]))),
                   Forall(disj (Rel("P",[])) (Rel("Q",[Bound 0]))))
let fof5 = (Implies (Rel("Q", [Free 'x']), Forall (Rel("Q", [Bound 0]))))
(* ⊢ Q(x) → ∀Q(0) ? *)
