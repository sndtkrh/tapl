open Untyped
open Lexer
open Parser

let file = "test.lambda"
let main =
  let ic = open_in file in
  try
    let tokenlist = lex ic in
    let term = parse tokenlist in
    Printf.printf "- %s\n" (toString term);
    let term' = evalall term in
    Printf.printf "=> %s\n" (toString term');
  with e ->
    raise e
