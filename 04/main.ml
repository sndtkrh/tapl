open Untyped
open Lexer
open Parser

let rec convert = function
  STrue -> TmTrue ""
| SFalse -> TmFalse ""
| SIfThenElse (t0, t1, t2) ->
   TmIf ("", convert t0, convert t1, convert t2)
| SZero -> TmZero ""
| SSucc t0 -> TmSucc ("", convert t0)
| SPred t0 -> TmPred ("", convert t0)
| SIszero t0 -> TmIsZero ("", convert t0)

let file = "test.lambda"
let main =
  let ic = open_in file in
  try
    let tokenized = lex ic in
    let (t, _) = exp tokenized in
    let term = convert t in
    Printf.printf "- %s\n" (toString term);
    let term' = evalall term in
    Printf.printf "=> %s\n" (toString term');
  with e ->
    raise e
