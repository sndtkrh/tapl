type info = string

type term =
  TmTrue of info
| TmFalse of info
| TmIf of info * term * term * term

let rec isval = function
    TmTrue _ -> true
  | TmFalse _ -> false
  | _ -> false

exception NoRuleApplies
let rec eval1 = function
    TmIf (_, TmTrue _, t1, t2) -> t1
  | TmIf (_, TmFalse _, t1, t2) -> t2
  | TmIf (fi, t0, t1, t2) -> let t0' = eval1 t0 in TmIf(fi, t0', t1, t2)
  | _ -> raise NoRuleApplies

let rec toString = function
    TmTrue _ -> "true"
  | TmFalse _ -> "false"
  | TmIf (_, t0, t1, t2) ->
     "if " ^ (toString t0) ^ " then " ^ (toString t1) ^ " else " ^ (toString t2)

let main =
  let t = TmIf ("", TmTrue "", TmFalse "", TmTrue "") in
  let t' = eval1 t in
  Printf.printf "%s\n" (toString t);
  Printf.printf "%s\n" (toString t')
