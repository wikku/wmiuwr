type name = string

type tm =
  | Var of name
  | App of tm * tm
  | Lam of name * tm
  | True | False
  | If of tm * tm * tm
  | Ann of tm * ty

and ty = Bool | Arr of ty * ty

type ctx = (name * ty) list
let lookup = List.assoc
let update x t ctx = (x,t) :: ctx
let empty = []

let rec infer_type ctx t = match t with
  | Var x -> lookup x ctx
  | True | False -> Bool
  | App (t1, t2) ->
    begin match infer_type ctx t1 with
    | Arr (ty1, ty2) ->
      if check_type ctx t2 ty2 then ty2 else failwith "wrong arg type"
    | _ -> failwith "application to non-arrow type"
    end
  | Ann (t, ty) ->
    if check_type ctx t ty then ty else failwith "wrong ann"
  | Lam _ -> failwith "cannot infer abs"
  | If _ -> failwith "cannot infer if"
and check_type ctx t ty = match t with
  | If (t1, t2, t3) ->
    check_type ctx t1 Bool && check_type ctx t2 ty && check_type ctx t3 ty
  | Lam (x, t') ->
    begin match ty with
    | Arr (ty1, ty2) -> check_type (update x ty1 ctx) t' ty2
    | _ -> failwith "abs must have arrow type"
    end
  | t -> infer_type ctx t = ty

let _ = infer_type [] (Ann (If (True, False, True), Bool))
let _ = infer_type [] (Ann (Lam ("b", If (Var "b", False, True)), Arr (Bool, Bool)))
