type t =
  TmTrue
| TmFalse
| TmIf of t * t * t
| TmZero
| TmSucc of t
| TmPred of t
| TmIsZero of t

let rec isnumericalval = function
    TmZero -> true
  | TmSucc t1 -> isnumericalval t1
  | _ -> false

let rec isval = function
    TmTrue -> true
  | TmFalse -> false
  | t when isnumericalval t -> true
  | _ -> false

exception NoRuleApplies
let rec eval1 = function
    TmIf (TmTrue, t1, t2) ->
     t1
  | TmIf (TmFalse, t1, t2) ->
     t2
  | TmIf (t0, t1, t2) ->
     let t0' = eval1 t0 in
     TmIf(t0', t1, t2)
  | TmSucc t0 ->
     let t0' =  eval1 t0 in
     TmSucc t0'
  | TmPred TmZero ->
     TmZero
  | TmPred (TmSucc nv1) when (isnumericalval nv1) ->
     nv1
  | TmPred t0 ->
     let t0' =  eval1 t0 in
     TmPred t0'
  | TmIsZero TmZero ->
     TmTrue
  | TmIsZero (TmSucc nv1) when (isnumericalval nv1) ->
     TmFalse
  | TmIsZero t1 ->
     let t1' = eval1 t1 in TmIsZero t1'
  | _ -> raise NoRuleApplies

let rec evalall t =
  if isval t
  then t
  else
    let t' = eval1 t in
    evalall t'

let rec toString = function
    TmTrue -> "true"
  | TmFalse -> "false"
  | TmIf (t0, t1, t2) ->
     "if " ^ (toString t0) ^ " then " ^ (toString t1) ^ " else " ^ (toString t2)
  | TmZero -> "0"
  | TmSucc t0 -> "succ " ^ (toString t0)
  | TmPred t0 -> "pred " ^ (toString t0)
  | TmIsZero t0 -> "iszero " ^ (toString t0)

