open Ast
open Tctxt

(* Use type_error to report error messages for ill-typed programs. *)
exception TypeError of string
let type_error (l : 'a node) err = 
  let (_, (s, e), _) = l.loc in
  raise (TypeError (Printf.sprintf "[%d, %d] %s" s e err))

(* The Oat types of the Oat built-in functions *)
let builtins =
  [ "array_of_string",  ([TRef RString],  RetVal (TRef(RArray TInt)))
  ; "string_of_array",  ([TRef(RArray TInt)], RetVal (TRef RString))
  ; "length_of_string", ([TRef RString],  RetVal TInt)
  ; "string_of_int",    ([TInt], RetVal (TRef RString))
  ; "string_cat",       ([TRef RString; TRef RString], RetVal (TRef RString))
  ; "print_string",     ([TRef RString],  RetVoid)
  ; "print_int",        ([TInt], RetVoid)
  ; "print_bool",       ([TBool], RetVoid)
  ]

(* binary operation types --------------------------------------------------- *)
let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Eq | Neq | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)

(* unary operation types ---------------------------------------------------- *)
let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)


(* expressions -------------------------------------------------------------- *)
(* TASK:

   Typechecks an expression in the typing context c, returns the type of the 
   expression.  This function should implement the inference rules given in
   the oad.pdf specification.  There, they are written:

       F; S; G; L |- exp : t

   See tctxt.ml for the implementation of the context c, which represents the
   four typing contexts:
        F - for function identifiers
        S - for structure definitions
        G - for global identifiers
        L - for local identifiers

   Notes:
     - Pay careful attention to the Id x case.  The abstract syntax treats
       function, global, and local identifiers all as Id x, but the 
       typechecking rules (and compilation invariants) treat function identifiers
       differently.

     - Structure values permit the programmer to write the fields in 
       any order (compared with the structure definition).  This means
       that, given the declaration 
          struct T { a:int; b:int; c:int } 
       The expression  
          new T {b=3; c=4; a=1}
       is well typed.  (You should sort the fields to compare them.)
       This is the meaning of the permutation pi that is used in the 
       TYP_STRUCTLIT rule.
*)
let rec typecheck_exp (c : Tctxt.t) (e : Ast.exp node) : Ast.ty =
  begin match e.elt with
  | CNull t -> 
    begin match t with
    | TRef _ -> t
    | _ -> type_error e "typecheck_exp: not a ref type for CNull"
    end
  | CBool b -> TBool
  | CInt n -> TInt
  | CStr s -> TRef RString
  | CArr (ty, exps) -> failwith "unimplemented"
  | CStruct (id, cfields) -> failwith "unimplemented"
  | Proj (exp, id) -> failwith "unimplemented"
  | NewArr (t, exp) -> failwith "unimplemented"
  | Id id -> failwith "unimplemented"
  | Index (exp1, exp2) -> failwith "unimplemented"
  | Call (exp, exps) -> failwith "unimplemented"
  | Bop (op, exp1, exp2) -> 
    let t1, t2, tret = typ_of_binop op in
      if (typecheck_exp c exp1) != t1 then
        type_error exp1 "typecheck_exp: invalid type for binop exp1"
      else
        if (typecheck_exp c exp2) != t2 then
          type_error exp2 "typecheck_exp: invalid type for binop exp2"
        else
          tret
      
  | Uop (op, exp) ->
    let t, tret = typ_of_unop op in
      if (typecheck_exp c exp) != t then
        type_error exp "typecheck_exp: invalid type for unop exp"
      else
        tret
  end


(* statements --------------------------------------------------------------- *)

(* return behavior of a statement:
     - NoReturn:  might not return
     - Return: definitely returns 
*)
type stmt_type = NoReturn | Return

(* TASK: Typecheck a statement 
     - to_ret is the desired return type (from the function declaration
    
   This function should implement the statment typechecking rules from oat.pdf.  
   
   - In the TYP_IF rule, the "sup" operation is the least-upper-bound operation on the 
     lattice of stmt_type values given by the reflexive relation, plus:
           Return <: NoReturn
     Intuitively: if one of the two branches of a conditional does not contain a 
     return statement, then the entier conditional statement might not return.

   - You will probably find it convenient to add a helper function that implements the 
     block typecheck rules.
*)
let rec typecheck_stmt (tc : Tctxt.t) (s:Ast.stmt node) (to_ret:ret_ty) : Tctxt.t * stmt_type =
  begin match s.elt with
  | Decl (id, exp) -> 
    let t = typecheck_exp tc exp in (add_local tc id t, NoReturn)
  | _ -> failwith "typecheck_stmt unimplemented"
  end


(* well-formed types -------------------------------------------------------- *)
(* TASK: Implement a (set of) functions that check that types are well formed.

    - l is just an ast node that provides source location information for
      generating error messages (it's only needed for type_error

    - tc contains the structure definition context
 *)
let rec typecheck_ty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ty) : unit =
  begin match t with
  | TBool | TInt -> ()
  | TRef rty -> typecheck_rty l tc rty
  end
and typecheck_rty (l : 'a Ast.node) (tc : Tctxt.t) (rt : Ast.rty) : unit =
  begin match rt with
  | RString -> ()
  | RStruct id -> 
    let fields = lookup_struct_option id tc in
      begin match fields with
      | None -> type_error l (String.concat " " ["tyepcheck_rty: ctxt does not contain"; id]) 
      | Some fs -> 
        let typecheck_rty_helper (f:Ast.field) : unit =  
          begin match f.ftyp with
          | TRef (RStruct id') -> 
            if id != id' then 
              typecheck_ty l tc f.ftyp
            else 
              ()
          | _ -> typecheck_ty l tc f.ftyp
          end in
        List.iter typecheck_rty_helper fs
      end
  | RArray ty -> typecheck_ty l tc ty
  | RFun fty -> typecheck_fty l tc fty
  end
and typecheck_fty (l : 'a Ast.node) (tc : Tctxt.t) (fty : Ast.fty) : unit = 
  let ty_list, ret_ty = fty in
    List.iter (fun t -> typecheck_ty l tc t) ty_list;
    typecheck_ret_ty l tc ret_ty
and typecheck_ret_ty (l : 'a Ast.node) (tc : Tctxt.t) (ret_ty : Ast.ret_ty) : unit =  
  begin match ret_ty with
  | RetVoid -> ()
  | RetVal ty -> typecheck_ty l tc ty
  end


let typecheck_tdecl (tc : Tctxt.t) l  (loc : 'a Ast.node) =
  List.iter (fun f -> typecheck_ty loc tc f.ftyp) l

(* function declarations ---------------------------------------------------- *)
(* TASK: typecheck a function declaration 
    - extends the local context with the types of the formal parameters to the 
      function
    - typechecks the body of the function (passing in the expected return type
    - checks that the function actually returns
*)
let rec typecheck_fbody (fbody:Ast.block) (tc:Tctxt.t) (ret_ty:Ast.ret_ty): unit = 
  begin match fbody with
  | [] -> ()
  | h::t -> 
  let new_tc, _ = typecheck_stmt tc h ret_ty in 
    typecheck_fbody t new_tc ret_ty
  end

let typecheck_fdecl (tc : Tctxt.t) (f : Ast.fdecl) (l : 'a Ast.node)  =
  let fty = lookup_function_option f.name tc in
    begin match fty with
    | None -> raise (Invalid_argument (String.concat " " ["undefined symbol"; f.name]))
    | Some (ty_list, ret_ty) ->
      let new_tc = 
        List.fold_left (fun acc (ty, id) -> Tctxt.add_local acc id ty) tc f.args in
      typecheck_fbody f.body new_tc ret_ty;
      let exist_fun (snode: Ast.stmt node) : bool = 
        begin match snode.elt with
        | Ret _ -> true
        | _ -> false 
        end in 
      if List.exists exist_fun f.body then 
        ()
      else 
        raise (Invalid_argument "function does not have return stmt")
    end

(* creating the typchecking context ----------------------------------------- *)

(* TASK: Complete the following functions that correspond to the
   judgments that create the global typechecking context.

   create_struct_ctxt: - adds all the struct types to the struct 'S'
   context (checking to see that there are no duplicate fields

   create_function_ctxt: - adds the the function identifiers and their
   types to the 'F' context (ensuring that there are no redeclared
   function identifiers)

   create_global_ctxt: - typechecks the global initializers and adds
   their identifiers to the 'G' global context

   NOTE: global initializers may mention function identifiers as
   constants, but can't mention other global values *)

(* Helper function to look for duplicate field names *)
let rec check_dups fs =
  match fs with
  | [] -> false
  | h :: t -> if List.exists (fun x -> x.fname = h.fname) t then true else check_dups t

let create_struct_ctxt p : Tctxt.t =
  let helper (acc:struct_ctxt) (decl:Ast.decl): struct_ctxt = 
    begin match decl with
    | Gtdecl tnode -> 
      let id, fields = tnode.elt in
        if check_dups fields then 
          type_error tnode "create_struct_ctxt: tdecl fields contains dup"
        else
          tnode.elt::acc
    | _ -> acc
    end in
  { locals = [];
    globals = [];
    functions = []; 
    structs = List.fold_left helper [] p;
  }
  


let create_function_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  let builtins_context = 
    List.fold_left (fun c (id, t) -> Tctxt.add_function c id t) tc builtins
  in
    let helper (acc: t) (decl:Ast.decl):t =
      begin match decl with
      |Gfdecl fnode ->
      add_function acc fnode.elt.name (List.map fst fnode.elt.args, fnode.elt.rtyp)
      | _ -> acc
      end in
    List.fold_left helper builtins_context p

let create_global_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  let helper (acc:t) (decl:Ast.decl) : t =
    begin match decl with
    | Gvdecl gnode -> 
      add_global acc gnode.elt.name (typecheck_exp acc gnode.elt.init)
    | _ -> acc
    end in
  List.fold_left helper tc p 

(* typechecks the whole program in the correct global context --------------- *)
(* This function implements the TYP_PROG rule of the oat.pdf specification.
   Note that global initializers are already checked in create_global_ctxt 
*)
let typecheck_program (p:Ast.prog) : unit =
  let sc = create_struct_ctxt p in
  let fc = create_function_ctxt sc p in
  let tc = create_global_ctxt fc p in
  List.iter (fun p ->
    match p with
    | Gfdecl ({elt=f} as l) -> typecheck_fdecl tc f l
    | Gtdecl ({elt=(id, fs)} as l) -> typecheck_tdecl tc fs l 
    | _ -> ()) p
