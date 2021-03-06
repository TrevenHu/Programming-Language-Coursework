type token =
  | Num of (int)
  | Id of (string)
  | EOF
  | LET
  | EQ
  | IN
  | FUN
  | PLUS
  | MUL
  | AND
  | OR
  | LPAREN
  | RPAREN
  | SEMI
  | COLONCOLON
  | TRUE
  | FALSE
  | REC
  | ARROW
  | IF
  | THEN
  | ELSE
  | MINUS
  | DIV
  | LT
  | LE
  | NE
  | LBRAC
  | RBRAC

open Parsing;;
let _ = parse_error;;
# 2 "nanoParse.mly"
(* See this for a tutorial on ocamlyacc
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano

let rec consAtTheEnd l e = match l with
  | NilExpr       -> Bin (e, Cons, NilExpr)
  | Bin(h, op, t) -> Bin (h, op,   consAtTheEnd t e)
# 43 "nanoParse.ml"
let yytransl_const = [|
    0 (* EOF *);
  259 (* LET *);
  260 (* EQ *);
  261 (* IN *);
  262 (* FUN *);
  263 (* PLUS *);
  264 (* MUL *);
  265 (* AND *);
  266 (* OR *);
  267 (* LPAREN *);
  268 (* RPAREN *);
  269 (* SEMI *);
  270 (* COLONCOLON *);
  271 (* TRUE *);
  272 (* FALSE *);
  273 (* REC *);
  274 (* ARROW *);
  275 (* IF *);
  276 (* THEN *);
  277 (* ELSE *);
  278 (* MINUS *);
  279 (* DIV *);
  280 (* LT *);
  281 (* LE *);
  282 (* NE *);
  283 (* LBRAC *);
  284 (* RBRAC *);
    0|]

let yytransl_block = [|
  257 (* Num *);
  258 (* Id *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\003\000\003\000\
\004\000\004\000\005\000\005\000\005\000\005\000\005\000\006\000\
\006\000\006\000\007\000\007\000\007\000\009\000\009\000\009\000\
\010\000\010\000\011\000\011\000\011\000\011\000\011\000\011\000\
\008\000\008\000\000\000"

let yylen = "\002\000\
\001\000\006\000\007\000\004\000\006\000\001\000\003\000\001\000\
\003\000\001\000\003\000\003\000\003\000\003\000\001\000\003\000\
\003\000\001\000\003\000\003\000\001\000\003\000\003\000\001\000\
\002\000\001\000\001\000\001\000\001\000\001\000\003\000\002\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\027\000\030\000\000\000\000\000\000\000\028\000\
\029\000\000\000\000\000\035\000\001\000\000\000\000\000\000\000\
\015\000\000\000\000\000\000\000\026\000\000\000\000\000\000\000\
\000\000\000\000\032\000\033\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\025\000\000\000\000\000\000\000\031\000\000\000\000\000\
\017\000\000\000\000\000\011\000\013\000\014\000\012\000\000\000\
\016\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\
\034\000\000\000\000\000\000\000\002\000\000\000\005\000\003\000"

let yydgoto = "\002\000\
\012\000\013\000\014\000\015\000\016\000\017\000\018\000\029\000\
\019\000\020\000\021\000"

let yysindex = "\011\000\
\012\255\000\000\000\000\000\000\008\255\033\255\012\255\000\000\
\000\000\012\255\005\255\000\000\000\000\037\255\044\255\045\255\
\000\000\251\254\011\255\041\255\000\000\046\255\052\255\040\255\
\048\255\042\255\000\000\000\000\009\255\063\255\063\255\063\255\
\063\255\063\255\063\255\041\255\063\255\041\255\041\255\041\255\
\038\255\000\000\012\255\068\255\012\255\000\000\012\255\012\255\
\000\000\044\255\045\255\000\000\000\000\000\000\000\000\011\255\
\000\000\011\255\041\255\041\255\062\255\012\255\000\000\054\255\
\000\000\012\255\072\255\012\255\000\000\012\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\153\000\185\000\173\000\
\000\000\151\000\076\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\198\000\179\000\000\000\000\000\000\000\000\000\101\000\
\000\000\126\000\026\000\051\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\249\255\000\000\000\000\043\000\053\000\048\000\000\000\000\000\
\010\000\246\255\241\255"

let yytablesize = 482
let yytable = "\025\000\
\024\000\036\000\026\000\028\000\042\000\003\000\004\000\005\000\
\037\000\022\000\006\000\001\000\003\000\004\000\005\000\007\000\
\038\000\006\000\039\000\008\000\009\000\048\000\007\000\010\000\
\023\000\022\000\008\000\009\000\059\000\060\000\010\000\011\000\
\027\000\040\000\024\000\061\000\049\000\063\000\011\000\064\000\
\065\000\003\000\004\000\042\000\042\000\056\000\030\000\058\000\
\032\000\043\000\023\000\007\000\031\000\044\000\067\000\008\000\
\009\000\045\000\069\000\046\000\071\000\047\000\072\000\003\000\
\004\000\027\000\066\000\041\000\033\000\034\000\035\000\062\000\
\050\000\007\000\068\000\021\000\070\000\008\000\009\000\052\000\
\053\000\054\000\055\000\051\000\057\000\000\000\000\000\000\000\
\000\000\011\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\019\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\020\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\018\000\000\000\
\006\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\010\000\000\000\000\000\000\000\
\000\000\000\000\009\000\000\000\000\000\000\000\000\000\000\000\
\008\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\007\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\024\000\024\000\000\000\024\000\
\024\000\024\000\024\000\000\000\024\000\024\000\024\000\000\000\
\000\000\000\000\000\000\000\000\024\000\024\000\024\000\024\000\
\024\000\024\000\024\000\000\000\024\000\022\000\022\000\000\000\
\022\000\022\000\022\000\022\000\000\000\022\000\022\000\022\000\
\000\000\000\000\000\000\000\000\000\000\022\000\022\000\022\000\
\022\000\022\000\022\000\022\000\000\000\022\000\023\000\023\000\
\000\000\023\000\023\000\023\000\023\000\000\000\023\000\023\000\
\023\000\000\000\000\000\000\000\000\000\000\000\023\000\023\000\
\023\000\023\000\023\000\023\000\023\000\000\000\023\000\021\000\
\021\000\000\000\021\000\000\000\021\000\021\000\000\000\021\000\
\021\000\021\000\000\000\000\000\000\000\000\000\000\000\021\000\
\021\000\021\000\000\000\021\000\021\000\021\000\000\000\021\000\
\019\000\019\000\000\000\019\000\000\000\019\000\019\000\000\000\
\019\000\019\000\019\000\000\000\000\000\000\000\000\000\000\000\
\019\000\019\000\019\000\000\000\019\000\019\000\019\000\000\000\
\019\000\020\000\020\000\000\000\020\000\000\000\020\000\020\000\
\000\000\020\000\020\000\020\000\000\000\000\000\000\000\000\000\
\000\000\020\000\020\000\020\000\000\000\020\000\020\000\020\000\
\000\000\020\000\018\000\018\000\000\000\006\000\000\000\018\000\
\018\000\000\000\018\000\018\000\006\000\006\000\000\000\000\000\
\000\000\000\000\018\000\018\000\006\000\006\000\018\000\018\000\
\018\000\010\000\018\000\000\000\006\000\010\000\010\000\009\000\
\010\000\010\000\000\000\009\000\009\000\008\000\009\000\009\000\
\010\000\010\000\008\000\000\000\008\000\008\000\009\000\009\000\
\010\000\000\000\007\000\000\000\008\000\008\000\009\000\007\000\
\000\000\007\000\007\000\000\000\008\000\000\000\000\000\000\000\
\000\000\007\000\007\000\000\000\000\000\000\000\000\000\000\000\
\000\000\007\000"

let yycheck = "\007\000\
\000\000\007\001\010\000\011\000\020\000\001\001\002\001\003\001\
\014\001\002\001\006\001\001\000\001\001\002\001\003\001\011\001\
\022\001\006\001\008\001\015\001\016\001\013\001\011\001\019\001\
\017\001\000\000\015\001\016\001\039\000\040\000\019\001\027\001\
\028\001\023\001\002\001\043\000\028\001\045\000\027\001\047\000\
\048\000\001\001\002\001\059\000\060\000\036\000\010\001\038\000\
\004\001\004\001\000\000\011\001\009\001\002\001\062\000\015\001\
\016\001\018\001\066\000\012\001\068\000\020\001\070\000\001\001\
\002\001\028\001\005\001\027\001\024\001\025\001\026\001\004\001\
\030\000\011\001\021\001\000\000\005\001\015\001\016\001\032\000\
\033\000\034\000\035\000\031\000\037\000\255\255\255\255\255\255\
\255\255\027\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\004\001\005\001\255\255\007\001\
\008\001\009\001\010\001\255\255\012\001\013\001\014\001\255\255\
\255\255\255\255\255\255\255\255\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\255\255\028\001\004\001\005\001\255\255\
\007\001\008\001\009\001\010\001\255\255\012\001\013\001\014\001\
\255\255\255\255\255\255\255\255\255\255\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\255\255\028\001\004\001\005\001\
\255\255\007\001\008\001\009\001\010\001\255\255\012\001\013\001\
\014\001\255\255\255\255\255\255\255\255\255\255\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\255\255\028\001\004\001\
\005\001\255\255\007\001\255\255\009\001\010\001\255\255\012\001\
\013\001\014\001\255\255\255\255\255\255\255\255\255\255\020\001\
\021\001\022\001\255\255\024\001\025\001\026\001\255\255\028\001\
\004\001\005\001\255\255\007\001\255\255\009\001\010\001\255\255\
\012\001\013\001\014\001\255\255\255\255\255\255\255\255\255\255\
\020\001\021\001\022\001\255\255\024\001\025\001\026\001\255\255\
\028\001\004\001\005\001\255\255\007\001\255\255\009\001\010\001\
\255\255\012\001\013\001\014\001\255\255\255\255\255\255\255\255\
\255\255\020\001\021\001\022\001\255\255\024\001\025\001\026\001\
\255\255\028\001\004\001\005\001\255\255\005\001\255\255\009\001\
\010\001\255\255\012\001\013\001\012\001\013\001\255\255\255\255\
\255\255\255\255\020\001\021\001\020\001\021\001\024\001\025\001\
\026\001\005\001\028\001\255\255\028\001\009\001\010\001\005\001\
\012\001\013\001\255\255\009\001\010\001\005\001\012\001\013\001\
\020\001\021\001\010\001\255\255\012\001\013\001\020\001\021\001\
\028\001\255\255\005\001\255\255\020\001\021\001\028\001\010\001\
\255\255\012\001\013\001\255\255\028\001\255\255\255\255\255\255\
\255\255\020\001\021\001\255\255\255\255\255\255\255\255\255\255\
\255\255\028\001"

let yynames_const = "\
  EOF\000\
  LET\000\
  EQ\000\
  IN\000\
  FUN\000\
  PLUS\000\
  MUL\000\
  AND\000\
  OR\000\
  LPAREN\000\
  RPAREN\000\
  SEMI\000\
  COLONCOLON\000\
  TRUE\000\
  FALSE\000\
  REC\000\
  ARROW\000\
  IF\000\
  THEN\000\
  ELSE\000\
  MINUS\000\
  DIV\000\
  LT\000\
  LE\000\
  NE\000\
  LBRAC\000\
  RBRAC\000\
  "

let yynames_block = "\
  Num\000\
  Id\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp8) in
    Obj.repr(
# 60 "nanoParse.mly"
                                       ( _1 )
# 303 "nanoParse.ml"
               : Nano.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 62 "nanoParse.mly"
                                       ( Let(_2,_4,_6) )
# 312 "nanoParse.ml"
               : 'exp8))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 64 "nanoParse.mly"
                                       ( Letrec(_3,_5,_7) )
# 321 "nanoParse.ml"
               : 'exp8))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 65 "nanoParse.mly"
                                       ( Fun(_2,_4) )
# 329 "nanoParse.ml"
               : 'exp8))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Nano.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 66 "nanoParse.mly"
                                       ( If(_2,_4,_6) )
# 338 "nanoParse.ml"
               : 'exp8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp7) in
    Obj.repr(
# 67 "nanoParse.mly"
                                       ( _1 )
# 345 "nanoParse.ml"
               : 'exp8))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp7) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp6) in
    Obj.repr(
# 69 "nanoParse.mly"
                                       ( Bin(_1,Or,_3) )
# 353 "nanoParse.ml"
               : 'exp7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp6) in
    Obj.repr(
# 70 "nanoParse.mly"
                                       ( _1 )
# 360 "nanoParse.ml"
               : 'exp7))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp6) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp5) in
    Obj.repr(
# 72 "nanoParse.mly"
                                       ( Bin(_1,And,_3) )
# 368 "nanoParse.ml"
               : 'exp6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp5) in
    Obj.repr(
# 73 "nanoParse.mly"
                                       ( _1 )
# 375 "nanoParse.ml"
               : 'exp6))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp5) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp54) in
    Obj.repr(
# 75 "nanoParse.mly"
                                       ( Bin(_1,Eq,_3) )
# 383 "nanoParse.ml"
               : 'exp5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp5) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp54) in
    Obj.repr(
# 77 "nanoParse.mly"
                                       ( Bin(_1,Ne,_3) )
# 391 "nanoParse.ml"
               : 'exp5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp5) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp54) in
    Obj.repr(
# 78 "nanoParse.mly"
                                       ( Bin(_1,Lt,_3) )
# 399 "nanoParse.ml"
               : 'exp5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp5) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp54) in
    Obj.repr(
# 79 "nanoParse.mly"
                                       ( Bin(_1,Le,_3) )
# 407 "nanoParse.ml"
               : 'exp5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp54) in
    Obj.repr(
# 80 "nanoParse.mly"
                                       ( _1 )
# 414 "nanoParse.ml"
               : 'exp5))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp4) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp54) in
    Obj.repr(
# 82 "nanoParse.mly"
                                       ( Bin(_1,Cons,_3) )
# 422 "nanoParse.ml"
               : 'exp54))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expseq) in
    Obj.repr(
# 83 "nanoParse.mly"
                                 ( _2 )
# 429 "nanoParse.ml"
               : 'exp54))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp4) in
    Obj.repr(
# 84 "nanoParse.mly"
                                       ( _1 )
# 436 "nanoParse.ml"
               : 'exp54))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp4) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp3) in
    Obj.repr(
# 86 "nanoParse.mly"
                                       ( Bin(_1,Plus,_3) )
# 444 "nanoParse.ml"
               : 'exp4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp4) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp3) in
    Obj.repr(
# 88 "nanoParse.mly"
                                       ( Bin(_1,Minus,_3) )
# 452 "nanoParse.ml"
               : 'exp4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp3) in
    Obj.repr(
# 89 "nanoParse.mly"
                                       ( _1 )
# 459 "nanoParse.ml"
               : 'exp4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp2) in
    Obj.repr(
# 91 "nanoParse.mly"
                                       ( Bin(_1,Mul,_3) )
# 467 "nanoParse.ml"
               : 'exp3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp2) in
    Obj.repr(
# 93 "nanoParse.mly"
                                       ( Bin(_1,Div,_3))
# 475 "nanoParse.ml"
               : 'exp3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp2) in
    Obj.repr(
# 94 "nanoParse.mly"
                                       ( _1 )
# 482 "nanoParse.ml"
               : 'exp3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp2) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp1) in
    Obj.repr(
# 96 "nanoParse.mly"
                                       ( App(_1,_2) )
# 490 "nanoParse.ml"
               : 'exp2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp1) in
    Obj.repr(
# 97 "nanoParse.mly"
                                       ( _1 )
# 497 "nanoParse.ml"
               : 'exp2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 99 "nanoParse.mly"
                                       ( Const _1 )
# 504 "nanoParse.ml"
               : 'exp1))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "nanoParse.mly"
                                       ( True )
# 510 "nanoParse.ml"
               : 'exp1))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "nanoParse.mly"
                                       ( False )
# 516 "nanoParse.ml"
               : 'exp1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 103 "nanoParse.mly"
                                       ( Var(_1) )
# 523 "nanoParse.ml"
               : 'exp1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Nano.expr) in
    Obj.repr(
# 104 "nanoParse.mly"
                                       ( _2 )
# 530 "nanoParse.ml"
               : 'exp1))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "nanoParse.mly"
                                       ( NilExpr )
# 536 "nanoParse.ml"
               : 'exp1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 107 "nanoParse.mly"
                                       ( consAtTheEnd NilExpr _1 )
# 543 "nanoParse.ml"
               : 'expseq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expseq) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 108 "nanoParse.mly"
                                       ( consAtTheEnd _1      _3 )
# 551 "nanoParse.ml"
               : 'expseq))
(* Entry exp *)
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
let exp (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Nano.expr)
