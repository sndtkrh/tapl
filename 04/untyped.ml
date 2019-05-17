type info = string

type term =
  TmTrue of info
| TmFalse of info
| TmIf of info * term * term * term
| TmZero of info
| TmSucc of info * term
| TmPred of info * term
| TmIsZero of info * term

let rec isnumericalval = function
    TmZero _ -> true
  | TmSucc (_, t1) -> isnumericalval t1
  | _ -> false

let rec isval = function
    TmTrue _ -> true
  | TmFalse _ -> false
  | t when isnumericalval t -> true
  | _ -> false

let dummyinfo = "dummyinfo"
exception NoRuleApplies
let rec eval1 = function
    TmIf (_, TmTrue _, t1, t2) ->
     t1
  | TmIf (_, TmFalse _, t1, t2) ->
     t2
  | TmIf (fi, t0, t1, t2) ->
     let t0' = eval1 t0 in
     TmIf(fi, t0', t1, t2)
  | TmSucc (fi, t0) ->
     let t0' =  eval1 t0 in
     TmSucc(fi, t0')
  | TmPred (fi, TmZero(_)) ->
     TmZero(dummyinfo)
  | TmPred (_, TmSucc(_, nv1)) when (isnumericalval nv1) ->
     nv1
  | TmPred (fi, t0) ->
     let t0' =  eval1 t0 in
     TmPred(fi, t0')
  | TmIsZero (_, TmZero(_)) ->
     TmTrue dummyinfo
  | TmIsZero (_, TmSucc(_, nv1)) when (isnumericalval nv1) ->
     TmFalse dummyinfo
  | TmIsZero (fi, t1) ->
     let t1' = eval1 t1 in TmIsZero(fi, t1')
  | _ -> raise NoRuleApplies

let rec evalall t =
  if isval t
  then t
  else
    let t' = eval1 t in
    evalall t'

let rec toString = function
    TmTrue _ -> "true"
  | TmFalse _ -> "false"
  | TmIf (_, t0, t1, t2) ->
     "if " ^ (toString t0) ^ " then " ^ (toString t1) ^ " else " ^ (toString t2)
  | TmZero _ -> "0"
  | TmSucc (_, t0) -> "succ " ^ (toString t0)
  | TmPred (_, t0) -> "pred " ^ (toString t0)
  | TmIsZero (_, t0) -> "iszero " ^ (toString t0)

                      (*
let main =
  let t = TmIf ("", TmTrue "", TmFalse "", TmTrue "") in
  let t' = eval1 t in
  Printf.printf "%s\n" (toString t);
  Printf.printf "%s\n" (toString t')
                       *)
