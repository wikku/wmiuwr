open Logic

let id = let f = Var 'p' in imp_i f (by_assumption f)

let const = by_assumption (Var 'p') |> imp_i (Var 'q') |> imp_i (Var 'p')

let thm3 =
  let p = Var 'p' and q = Var 'q' and r = Var 'r' in
  let paq = Implies (p, q) and paqar = Implies(p, Implies(q, r)) in
  let t1 = imp_e (by_assumption paqar) (by_assumption p) in
  let t2 = imp_e (by_assumption paq) (by_assumption p) in
  imp_e t1 t2 |> imp_i p |> imp_i paq |> imp_i paqar

let ex_falso = by_assumption Falsum |> bot_e (Var 'p') |> imp_i Falsum

