%{
open Ast

let loc (startpos:Lexing.position) (endpos:Lexing.position) (elt:'a) : 'a node =
  { elt ; loc=Range.mk_lex_range startpos endpos }

%}

/* Declare your tokens here. */
%token EOF
%token <int64>  INT
%token NULL
%token <string> STRING
%token <string> IDENT
%token TINT     /* int */
%token TVOID    /* void */
%token TSTRING  /* string */
%token TBOOL    /* bool */
%token IF       /* if */
%token ELSE     /* else */
%token NEW      /* new */
%token WHILE    /* while */
%token FOR      /* for */
%token RETURN   /* return */
%token <bool> BOOL     /* boolean */
%token VAR      /* var */
%token SEMI     /* ; */
%token COMMA    /* , */
%token LBRACE   /* { */
%token RBRACE   /* } */
%token PLUS     /* + */
%token DASH     /* - */
%token STAR     /* * */
%token EQEQ     /* == */
%token EQ       /* = */
%token LPAREN   /* ( */
%token RPAREN   /* ) */
%token LBRACKET /* [ */
%token RBRACKET /* ] */
%token TILDE    /* ~ */
%token BANG     /* ! */
%token GLOBAL   /* global */
%token LSHIFT   /* << */
%token RSHIFT   /* >> */
%token RSHIFTL  /* >>> */
%token LT       /* < */
%token LEQ      /* <= */
%token GT       /* > */
%token GEQ      /* >= */
%token NEQ      /* != */
%token AND      /* & */
%token OR       /* | */
%token BITAND   /* [&] */
%token BITOR    /* [|] */

%left BITOR
%left BITAND
%left OR
%left AND
%left EQEQ NEQ
%left LT LEQ GT GEQ 
%left LSHIFT RSHIFT RSHIFTL
%left PLUS DASH
%left STAR
%nonassoc BANG
%nonassoc TILDE
%nonassoc LBRACKET
/* STUBWITH */

/* ---------------------------------------------------------------------- */

%start prog
%start exp_top
%start stmt_top
%type <Ast.exp Ast.node> exp_top
%type <Ast.stmt Ast.node> stmt_top

%type <Ast.prog> prog
%type <Ast.exp Ast.node> exp
%type <Ast.stmt Ast.node> stmt
%type <Ast.block> block
%type <Ast.ty> ty
%%

exp_top:
  | e=exp EOF { e }

stmt_top:
  | s=stmt EOF { s }

prog:
  | p=list(decl) EOF  { p }

decl:
  | GLOBAL name=IDENT EQ init=gexp SEMI
    { Gvdecl (loc $startpos $endpos { name; init }) }
  | rtyp=ty name=IDENT LPAREN args=arglist RPAREN body=block
    { Gfdecl (loc $startpos $endpos { rtyp; name; args; body }) }

arglist:
  | l=separated_list(COMMA, pair(ty,IDENT)) { l }
    
ty:
  | TVOID  { TVoid }
  | TBOOL  { TBool }
  | TINT   { TInt }
  | r=rtyp { TRef r }



rtyp:
  | TSTRING { RString }
  | t=ty LBRACKET RBRACKET { RArray t }

%inline bop:
  | PLUS   { Add }
  | DASH   { Sub }
  | STAR   { Mul }
  | EQEQ   { Eq } 
  | LSHIFT { Shl }
  | RSHIFT { Sar }
  | RSHIFTL { Shr }
  | LT { Lt }
  | LEQ { Lte }
  | GT { Gt }
  | GEQ { Gte }
  | NEQ { Neq }
  | AND { And }
  | OR { Or }
  | BITAND { IAnd }
  | BITOR { IOr }

%inline uop:
  | DASH  { Neg }
  | BANG  { Lognot }
  | TILDE { Bitnot }

gexp:
  | t=ty NULL  { loc $startpos $endpos @@ CNull t }
  | i=INT      { loc $startpos $endpos @@ CInt i } 
  | s=STRING   { loc $startpos $endpos @@ CStr s }
  | t=BOOL     { loc $startpos $endpos @@ CBool t } 
  | t=ty LBRACKET RBRACKET LBRACE es=separated_list(COMMA, gexp) RBRACE
               { loc $startpos $endpos @@ CArr (t, es) }


lhs:  
  | id=IDENT            { loc $startpos $endpos @@ Id id }
  | e=exp LBRACKET i=exp RBRACKET
                        { loc $startpos $endpos @@ Index (e, i) }
exp:
  | i=INT               { loc $startpos $endpos @@ CInt i }
  | s=STRING            { loc $startpos $endpos @@ CStr s }
  | t=ty NULL           { loc $startpos $endpos @@ CNull t }
  | e1=exp b=bop e2=exp { loc $startpos $endpos @@ Bop (b, e1, e2) }
  | u=uop e=exp         { loc $startpos $endpos @@ Uop (u, e) }
  | id=IDENT            { loc $startpos $endpos @@ Id id }
  | e=exp LBRACKET i=exp RBRACKET
                        { loc $startpos $endpos @@ Index (e, i) }
  | id=IDENT LPAREN es=separated_list(COMMA, exp) RPAREN
                        { loc $startpos $endpos @@ Call (id,es) }
  | NEW t=ty LBRACKET RBRACKET LBRACE es=separated_list(COMMA, exp) RBRACE
                        { loc $startpos $endpos @@ CArr (t, es) }
  | NEW t=ty LBRACKET e=exp RBRACKET
                        { loc $startpos $endpos @@ NewArr (t, e) } 
  | LPAREN e=exp RPAREN { e }
  | t=BOOL { loc $startpos $endpos @@ CBool t } 

vdecl:
  | VAR id=IDENT EQ init=exp { (id, init) }

stmt: 
  | d=vdecl SEMI        { loc $startpos $endpos @@ Decl(d) }
  | p=lhs EQ e=exp SEMI { loc $startpos $endpos @@ Assn(p,e) }
  | id=IDENT LPAREN es=separated_list(COMMA, exp) RPAREN SEMI
                        { loc $startpos $endpos @@ SCall (id, es) }
  | ifs=if_stmt         { ifs }
  | RETURN SEMI         { loc $startpos $endpos @@ Ret(None) }
  | RETURN e=exp SEMI   { loc $startpos $endpos @@ Ret(Some e) }
  | WHILE LPAREN e=exp RPAREN b=block  
                        { loc $startpos $endpos @@ While(e, b) } 
  | FOR LPAREN vdecls=separated_list(COMMA, vdecl) SEMI SEMI RPAREN b=block
                        { loc $startpos $endpos @@ For(vdecls, None, None, b) }
  | FOR LPAREN vdecls=separated_list(COMMA, vdecl) SEMI e=exp SEMI s=stmt RPAREN b=block
                        { loc $startpos $endpos @@ For(vdecls, Some e, Some s, b) }

block:
  | LBRACE stmts=list(stmt) RBRACE { stmts }

if_stmt:
  | IF LPAREN e=exp RPAREN b1=block b2=else_stmt
    { loc $startpos $endpos @@ If(e,b1,b2) }

else_stmt:
  | (* empty *)       { [] }
  | ELSE b=block      { b }
  | ELSE ifs=if_stmt  { [ ifs ] }
