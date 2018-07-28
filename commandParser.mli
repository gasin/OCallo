type token =
  | INT of (int)
  | OPEN
  | END
  | MOVE
  | START
  | ACK
  | BYE
  | WIN
  | LOSE
  | TIE
  | WHITE
  | BLACK
  | NL
  | EOF
  | STR of (string)
  | SPACE

val comm :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Command.command
