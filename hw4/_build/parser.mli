
(* The type of tokens. *)

type token = 
  | WHILE
  | VAR
  | TVOID
  | TSTRING
  | TRUE of (bool)
  | TINT
  | TILDE
  | TBOOL
  | STRING of (string)
  | STAR
  | SEMI
  | RSHIFTL
  | RSHIFT
  | RPAREN
  | RETURN
  | RBRACKET
  | RBRACE
  | PLUS
  | OR
  | NULL
  | NEQ
  | LT
  | LSHIFT
  | LPAREN
  | LEQ
  | LBRACKET
  | LBRACE
  | INT of (int64)
  | IF
  | IDENT of (string)
  | GT
  | GLOBAL
  | GEQ
  | FALSE of (bool)
  | EQEQ
  | EQ
  | EOF
  | ELSE
  | DASH
  | COMMA
  | BITOR
  | BITAND
  | BANG
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val stmt_top: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.stmt Ast.node)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.prog)

val exp_top: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.exp Ast.node)
