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
val lex : in_channel -> t list
