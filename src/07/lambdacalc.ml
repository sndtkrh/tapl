type info = string
type term = 
    TmVar of info * int * int
  | TmAbs of info * string * term
  | TmApp of info * term * term

let rec pickfreshname ctx x =
    if List.mem x ctx then
        pickfreshname ctx (x^x)
    else
        (x :: ctx, x)

let rec printtm ctx t = match t with
    TmAbs(fi, x, t1) ->
      let (ctx', x') = pickfreshname ctx x in
      "(lambda " ^ x' ^ ". " ^ printtm ctx' t1 ^ ")"
  | TmApp(fi, t1, t2) ->
      "(" ^ printtm ctx t1 ^ " " ^ printtm ctx t2 ^ ")"
  | TmVar(fi, x, n) ->
      if List.length ctx = n then
          List.nth ctx x
      else
          "[bad index]"

type binding = NameBind
type context = (string * binding) list

let termShift d t = 
  let rec walk c t = match t with
      TmVar(fi, x, n) -> if x >= c then TmVar(fi, x+d, n+d) else TmVar(fi, x, n+d)
    | TmAbs(fi, x, t1) -> TmAbs(fi, x, walk (c+1) t1)
    | TmApp(fi, t1, t2) -> TmApp(fi, walk c t1, walk c t2)
  in walk 0 t

let termSubst j s t =
  let rec walk c t = match t with
       TmVar(fi, x, n) -> if x = j+c then termShift c s else TmVar(fi, x, n)
     | TmAbs(fi, x, t1) -> TmAbs(fi, x, walk (c+1) t1)
     | TmApp(fi, t1, t2) -> TmApp(fi, walk c t1, walk c t2)
  in walk 0 t

let termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)

let rec isval ctx t = match t with
    TmAbs(_, _, _) -> true
  | _ -> false

exception NoRuleApplies
let rec eval1 ctx t = match t with
    TmApp(fi, TmAbs(_, x, t12), v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp(fi, v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2)
  | _ ->
      raise NoRuleApplies

let rec eval ctx t =
    try let t' = eval1 ctx t
        in eval ctx t'
    with NoRuleApplies -> t

