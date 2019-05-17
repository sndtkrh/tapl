type t =
  TmTrue
| TmFalse
| TmIf of t * t * t
| TmZero
| TmSucc of t
| TmPred of t
| TmIsZero of t

exception NoRuleApplies
val evalall : t -> t
val toString : t -> string
