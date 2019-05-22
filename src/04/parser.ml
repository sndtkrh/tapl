exception ParseError

let rec expression = function
    [] -> raise ParseError
  | l ->
     let h = List.hd(l) in
     let tail = List.tl(l) in
     match h with
       Lexer.True -> (Untyped.TmTrue, tail)
     | Lexer.False -> (Untyped.TmFalse, tail)
     | Lexer.If -> ifthenelse tail
     | Lexer.Then -> raise ParseError
     | Lexer.Else -> raise ParseError
     | Lexer.Zero -> (Untyped.TmZero, tail)
     | Lexer.Succ ->
        let (t0, tokenlist0) = expression tail in
        (Untyped.TmSucc t0, tokenlist0)
     | Lexer.Pred ->
        let (t0, tokenlist0) = expression tail in
        (Untyped.TmPred t0, tokenlist0)
     | Lexer.IsZero ->
        let (t0, tokenlist0) = expression tail in
        (Untyped.TmIsZero t0, tokenlist0)

and ifthenelse tokenlist =
  let (t0, tokenlist0) = expression tokenlist in
  let tokenThen = List.hd tokenlist0 in
  if tokenThen <> Lexer.Then then raise ParseError;
  let (t1, tokenlist1) = expression (List.tl tokenlist0) in
  let tokenElse = List.hd tokenlist1 in
  if tokenElse <> Lexer.Else then raise ParseError;
  let (t2, tokenlist2) = expression (List.tl tokenlist1) in
  (Untyped.TmIf (t0, t1, t2), tokenlist2)
  
let parse tokenlist =
  let (tree, l) = expression tokenlist in
  if l <> []
  then raise ParseError
  else tree
