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

open Parsing;;
let _ = parse_error;;
let yytransl_const = [|
  258 (* OPEN *);
  259 (* END *);
  260 (* MOVE *);
  261 (* START *);
  262 (* ACK *);
  263 (* BYE *);
  264 (* WIN *);
  265 (* LOSE *);
  266 (* TIE *);
  267 (* WHITE *);
  268 (* BLACK *);
  269 (* NL *);
    0 (* EOF *);
  271 (* SPACE *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  270 (* STR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\003\000\
\003\000\003\000\004\000\004\000\005\000\005\000\002\000\002\000\
\000\000"

let yylen = "\002\000\
\003\000\006\000\003\000\005\000\003\000\003\000\001\000\001\000\
\001\000\001\000\001\000\001\000\004\000\005\000\001\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\015\000\016\000\017\000\007\000\000\000\008\000\009\000\010\000\
\000\000\000\000\011\000\012\000\000\000\000\000\000\000\000\000\
\001\000\000\000\003\000\000\000\005\000\000\000\006\000\000\000\
\000\000\000\000\000\000\004\000\000\000\002\000\014\000"

let yydgoto = "\002\000\
\011\000\012\000\017\000\021\000\024\000"

let yysindex = "\009\000\
\001\000\000\000\246\254\254\254\000\255\001\255\014\255\002\255\
\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\
\016\255\002\000\000\000\000\000\004\255\002\000\018\255\002\000\
\000\000\020\255\000\000\022\255\000\000\023\255\000\000\011\255\
\002\000\025\255\002\000\000\000\002\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\003\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\243\255\000\000\000\000\246\255"

let yytablesize = 272
let yytable = "\025\000\
\010\000\010\000\013\000\013\000\027\000\014\000\015\000\016\000\
\029\000\001\000\031\000\019\000\020\000\018\000\022\000\023\000\
\026\000\028\000\030\000\036\000\032\000\038\000\033\000\034\000\
\035\000\037\000\039\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\003\000\004\000\005\000\006\000\007\000\008\000\
\000\000\000\000\000\000\000\000\000\000\009\000\009\000\013\000"

let yycheck = "\013\000\
\000\000\000\000\000\000\014\001\018\000\008\001\009\001\010\001\
\022\000\001\000\024\000\011\001\012\001\014\001\001\001\014\001\
\001\001\014\001\001\001\033\000\001\001\035\000\001\001\001\001\
\014\001\001\001\037\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\004\001\005\001\006\001\007\001\
\255\255\255\255\255\255\255\255\255\255\013\001\013\001\013\001"

let yynames_const = "\
  OPEN\000\
  END\000\
  MOVE\000\
  START\000\
  ACK\000\
  BYE\000\
  WIN\000\
  LOSE\000\
  TIE\000\
  WHITE\000\
  BLACK\000\
  NL\000\
  EOF\000\
  SPACE\000\
  "

let yynames_block = "\
  INT\000\
  STR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eoc) in
    Obj.repr(
# 15 "commandParser.mly"
               ( Command.Open _2 )
# 184 "commandParser.ml"
               : Command.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'wl) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'eoc) in
    Obj.repr(
# 16 "commandParser.mly"
                         ( Command.End (_2,_3,_4,_5) )
# 195 "commandParser.ml"
               : Command.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eoc) in
    Obj.repr(
# 17 "commandParser.mly"
               ( let s = _2 in
             if s = "PASS" then Command.Move Command.Pass
             else if s = "GIVEUP" then Command.Move Command.GiveUp
             else if String.length s = 2 && 'A' <= s.[0] && s.[0] <= 'H' 
                  && '1' <= s.[1] && s.[1] <= '8' then 
               Command.Move 
                 (Command.Mv (int_of_char s.[0] - int_of_char 'A' + 1, 
                              int_of_char s.[1] - int_of_char '1' + 1))
             else
               failwith "Invalid Command" )
# 212 "commandParser.ml"
               : Command.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'wb) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'eoc) in
    Obj.repr(
# 27 "commandParser.mly"
                        ( Command.Start (_2,_3,_4) )
# 222 "commandParser.ml"
               : Command.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eoc) in
    Obj.repr(
# 28 "commandParser.mly"
                        ( Command.Ack   _2 )
# 230 "commandParser.ml"
               : Command.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'scores) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eoc) in
    Obj.repr(
# 29 "commandParser.mly"
                        ( Command.Bye   _2 )
# 238 "commandParser.ml"
               : Command.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eoc) in
    Obj.repr(
# 30 "commandParser.mly"
                        ( Command.Empty )
# 245 "commandParser.ml"
               : Command.command))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "commandParser.mly"
       ( Command.Win )
# 251 "commandParser.ml"
               : 'wl))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "commandParser.mly"
       ( Command.Lose )
# 257 "commandParser.ml"
               : 'wl))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "commandParser.mly"
       ( Command.Tie )
# 263 "commandParser.ml"
               : 'wl))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "commandParser.mly"
        ( Color.white )
# 269 "commandParser.ml"
               : 'wb))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "commandParser.mly"
        ( Color.black )
# 275 "commandParser.ml"
               : 'wb))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 45 "commandParser.mly"
                         ( [(_1,(_2,_3,_4))] )
# 285 "commandParser.ml"
               : 'scores))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'scores) in
    Obj.repr(
# 46 "commandParser.mly"
                         ( (_1,(_2,_3,_4)) :: _5 )
# 296 "commandParser.ml"
               : 'scores))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "commandParser.mly"
        ()
# 302 "commandParser.ml"
               : 'eoc))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "commandParser.mly"
        ()
# 308 "commandParser.ml"
               : 'eoc))
(* Entry comm *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let comm (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Command.command)
