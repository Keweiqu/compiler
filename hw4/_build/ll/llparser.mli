
(* The type of tokens. *)

type token = 
  | XOR
  | VOID
  | UID of (string)
  | TYPE
  | TO
  | SUB
  | STRING of (string)
  | STORE
  | STAR
  | SLT
  | SLE
  | SHL
  | SGT
  | SGE
  | RPAREN
  | RET
  | RBRACKET
  | RBRACE
  | OR
  | NULL
  | NE
  | MUL
  | LSHR
  | LPAREN
  | LOAD
  | LBRACKET
  | LBRACE
  | LBL of (string)
  | LABEL
  | INT of (int)
  | ICMP
  | I8
  | I64
  | I32
  | I1
  | GLOBAL
  | GID of (string)
  | GEP
  | EXTERNAL
  | EQUALS
  | EQ
  | EOF
  | ENTRY
  | DEFINE
  | DECLARE
  | CROSS
  | COMMA
  | COLON
  | CALL
  | BR
  | BITCAST
  | ASHR
  | AND
  | ALLOCA
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ll.prog)
