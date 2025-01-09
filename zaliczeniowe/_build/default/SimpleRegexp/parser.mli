type token =
  | CHAR of (char)
  | UNION
  | CONCAT
  | STAR
  | EMPTY
  | EPSILON
  | LPAR
  | RPAR
  | EOF
  | BADTOK

val regex :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> char SimpleRegexpDef.reg
