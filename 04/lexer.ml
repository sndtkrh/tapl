open Untyped

type t =
  True
| False
| If
| Then
| Else
| Zero
| Succ
| Pred
| Iszero

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
  | "iszero" -> Iszero
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

let token2String = function
    True -> "true"
  | False -> "false"
  | If -> "if"
  | Then -> "then"
  | Else -> "else"
  | Zero -> "0"
  | Succ -> "succ"
  | Pred -> "pred"
  | Iszero -> "iszero"
  
let tokenlist2String l =
  let l' = List.map token2String l in
  List.fold_left (fun s0 s1 -> s0 ^ " " ^ s1) "" l'

(*
let file = "test.lambda"
let main =
  let ic = open_in file in
  try
    let tokenized = lex ic in
    Printf.printf "%s\n" (tokenlist2String tokenized);
    close_in ic
  with e ->
    close_in_noerr ic;
    raise e
 *)
