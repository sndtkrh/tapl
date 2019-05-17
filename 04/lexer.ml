type t =
  True
| False
| If
| Then
| Else
| Zero
| Succ
| Pred
| IsZero

exception InvalidToken

let lex_one_word = function
    "true" -> True
  | "false" -> False
  | "if" -> If
  | "then" -> Then
  | "else" -> Else
  | "0" -> Zero
  | "succ" -> Succ
  | "pred" -> Pred
  | "iszero" -> IsZero
  | _ -> raise InvalidToken
        
let lex_one_line l =
  let l' = Str.split (Str.regexp "[ \t]+") l in
  List.map lex_one_word l'

let rec lex ic =
  try
    let line = input_line ic in
    let tokenized = lex_one_line line in
    List.append tokenized (lex ic)
  with End_of_file ->
    []
