
(* The type of tokens. *)

type token = 
  | WHILE
  | VAR
  | TVOID
  | TSTRING
  | TINT
  | TILDE
  | STRING of (string)
  | STAR
  | SEMI
  | RPAREN
  | RETURN
  | RBRACKET
  | RBRACE
  | PLUS
  | NULL
  | LPAREN
  | LBRACKET
  | LBRACE
  | INT of (int64)
  | IF
  | IDENT of (string)
  | GLOBAL
  | EQEQ
  | EQ
  | EOF
  | ELSE
  | DASH
  | COMMA
  | BANG

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val stmt_top: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.stmt Ast.node)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.prog)

val exp_top: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.exp Ast.node)
