open Lexer

type syntaxTree =
  STrue
| SFalse
| SIfThenElse of syntaxTree * syntaxTree * syntaxTree
| SZero
| SSucc of syntaxTree
| SPred of syntaxTree
| SIszero of syntaxTree


exception ParseError
let rec exp = function
    [] -> raise ParseError
  | l ->
     let h = List.hd(l) in
     let tail = List.tl(l) in
     match h with
       True -> (STrue, tail)
     | False -> (SFalse, tail)
     | If -> ifthenelse tail
     | Then -> raise ParseError
     | Else -> raise ParseError
     | Zero -> (SZero, tail)
     | Succ ->
        let (t0, tokenlist0) = exp tail in
        (SSucc t0, tokenlist0)
     | Pred ->
        let (t0, tokenlist0) = exp tail in
        (SPred t0, tokenlist0)
     | Iszero ->
        let (t0, tokenlist0) = exp tail in
        (SIszero t0, tokenlist0)
and ifthenelse tokenlist =
  let (t0, tokenlist0) = exp tokenlist in
  let tokenThen = List.hd tokenlist0 in
  if tokenThen <> Then then raise ParseError;
  let (t1, tokenlist1) = exp (List.tl tokenlist0) in
  let tokenElse = List.hd tokenlist1 in
  if tokenElse <> Else then raise ParseError;
  let (t2, tokenlist2) = exp (List.tl tokenlist1) in
  (SIfThenElse (t0, t1, t2), tokenlist2)
  
let rec tree2string = function
    STrue -> "true"
  | SFalse -> "false"
  | SIfThenElse (t0, t1, t2) ->
     "ifthenelse("
     ^ (tree2string t0) ^ ", "
     ^ (tree2string t1) ^ ", "
     ^ (tree2string t2) ^ ")"
  | SZero -> "0"
  | SSucc t0 -> "succ(" ^ (tree2string t0) ^ ")"
  | SPred t0 -> "pred(" ^ (tree2string t0) ^ ")"
  | SIszero t0 -> "iszero(" ^ (tree2string t0) ^ ")"

                (*
let file = "test.lambda"
let main =
  let ic = open_in file in
  try
    let tokenized = lex ic in
    let (tree, r) = exp tokenized in
    Printf.printf "%s\n" (tree2string tree);
    close_in ic
  with e ->
    close_in_noerr ic;
    raise e
                 *)
