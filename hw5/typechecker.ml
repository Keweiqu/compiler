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
  | CArr (ty, exps) -> 
    List.iter (fun exp -> if typecheck_exp c exp <> ty then type_error e "typecheck_exp: Carr unmatched types" else ()) exps;
    TRef (RArray ty)
  | CStruct (id, cfields) -> 
    let f = lookup_struct_option id c in
      begin match f with
      | None -> type_error e "typecheck_exp: undefined struct symbol"
      | Some fields ->
        let sorted_fields = List.sort (fun a b -> String.compare a.fname b.fname) fields in
        let sorted_cfields = List.sort (fun a b -> String.compare a.cfname b.cfname) cfields in
        if List.length sorted_fields <> List.length
 sorted_cfields then
          type_error e "typecheck_exp: unmatched struct fields"
        else
          List.iter2 (fun a b -> 
                       if a.fname <> b.cfname then
                         type_error e "typecheck_exp: struct does not have this field"
                       else
                         if (typecheck_exp c b.cfinit) <> a.ftyp then
                           type_error e "typecheck_exp: struct field type unmatched"
                         else
                           ()
                     ) sorted_fields sorted_cfields;
        TRef (RStruct id)
      end
  | Proj (exp, id) -> 
     begin match typecheck_exp c exp with
     | TRef (RStruct s) -> 
        let struct_type = lookup_struct_option s c in
        begin match struct_type with
        | None -> type_error e "typecheck_exp: undefined struct type"
        | Some fields -> try (List.find 
                                (fun field -> if field.fname = id then true else false) fields).ftyp
                         with Not_found -> type_error e "typecheck_exp: undefined field name"
        end
     | _ -> type_error e "typecheck_exp: projection of a non-struct type"
     end
  | NewArr (t, exp) -> 
     if typecheck_exp c exp <> TInt then
       type_error e "typecheck_exp: new arr exp not of TInt"
     else
       TRef (RArray t)
  | Id id -> 
     begin match lookup_option id c with
     | None -> 
        begin match lookup_function_option id c with
        | None -> type_error e "typecheck_exp: undeclared symbol"
        | Some fty -> TRef (RFun fty)
        end
     | Some x -> x
     end
  | Index (exp1, exp2) -> 
     if typecheck_exp c exp2 <> TInt then
       type_error e "typecheck_exp: index not of type int"
     else
       begin match typecheck_exp c exp1 with
       | TRef (RArray ty) -> ty
       | _ -> type_error e "typecheck_exp: index into a non-array type"
       end
  | Call (exp, exps) -> 
     let call_ty = typecheck_exp c exp in
     begin match call_ty with
     | TRef (RFun fty) ->
        let ty_list, ret_ty = fty in
        if List.length ty_list <> List.length exps then
          type_error e "typecheck_exp: call number of args unmatched"
        else
          List.iter2 (fun a b -> 
                       if a <> typecheck_exp c b then
                         type_error e "typecheck_exp: call number of args unmatched"
                       else
                         ()) ty_list exps;
        begin match ret_ty with
        | RetVoid -> type_error e "typecheck_exp: call to a void return function"
        | RetVal t -> t
        end
     | _ -> type_error e "typecheck_exp: call to not a function"
     end
  | Bop (op, exp1, exp2) -> 
    let t1, t2, tret = typ_of_binop op in
      if (typecheck_exp c exp1) <> t1 then
        type_error exp1 "typecheck_exp: invalid type for binop exp1"
      else
        if (typecheck_exp c exp2) <> t2 then
          type_error exp2 "typecheck_exp: invalid type for binop exp2"
        else
          tret
  | Uop (op, exp) ->
    let t, tret = typ_of_unop op in
      if (typecheck_exp c exp) <> t then
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
  | Assn (lhs, rhs) ->
    let lhs_ty = typecheck_exp tc lhs in
    let rhs_ty = typecheck_exp tc rhs in
      if lhs_ty <> rhs_ty then
        type_error s "typecheck_stmt: assignment lhs and rhs do not match"
      else
        (tc, NoReturn)
  | Decl (id, exp) -> 
     let id_ty = lookup_option id tc in
     if id_ty <> None && id_ty <> Some (typecheck_exp tc exp) then
       type_error s "typecheck_stmt: overwrite variable declaration"
     else
       let t = typecheck_exp tc exp in (add_local tc id t, NoReturn)
  | Ret exp -> 
    begin match exp with
    | None -> 
      if to_ret = RetVoid then
        (tc, Return)
      else 
        type_error s "typecheck_stmt: void return returns value"
    | Some e -> 
      let e_ty = typecheck_exp tc e in
        if (RetVal e_ty) = to_ret then
          (tc, Return)
        else
          type_error s "typecheck_stmt: return type does not match"
    end
  | SCall (exp, exp_list) -> 
    begin match exp.elt with
    | Id id -> 
      let fty = lookup_function_option id tc in
        begin match fty with
        | None -> type_error s "typecheck_stmt: undefined function"
        | Some (ty_list, ret_ty) -> 
          if ret_ty = RetVoid then
            if List.length exp_list <> List.length ty_list then
              type_error s "typecheck_stmt: unmatched number of function args"
            else
              let _ = List.iter2 (fun exp expected_rt -> 
                                   if (typecheck_exp tc exp) <> expected_rt then
                                     type_error s "typecheck_stmt: function argument type not match"
                                   else ()
                                 ) exp_list ty_list in
              (tc, NoReturn)
          else 
            type_error s "typecheck_stmt: scall function not return void"
        end
    | _ -> raise (Invalid_argument "typecheck_stmt: fun first exp should be id")
    end
  | If (exp, b1, b2) ->
    let exp_ty = typecheck_exp tc exp in 
      if exp_ty <> TBool then
        type_error exp "typecheck_stmt: if condition must be a bool"
      else 
        let _, b1_stmt_ty = typecheck_block b1 tc to_ret NoReturn in 
        let _, b2_stmt_ty = typecheck_block b2 tc to_ret NoReturn in 
          if b1_stmt_ty = Return && b2_stmt_ty = Return then
            (tc, Return)
          else 
            (tc, NoReturn)
  | For (vdecl_list, None, None, b) ->
    let vdecls = List.map (fun a -> no_loc (Decl a)) vdecl_list in
      let ctxt2 = List.fold_left (fun acc stmt -> fst (typecheck_stmt acc stmt to_ret)) tc vdecls in
        let _ = typecheck_block b ctxt2 to_ret NoReturn in
    (tc, NoReturn)
  | For (vdecl_list, Some exp, Some stmt, b) ->
     let vdecls = List.map (fun a -> no_loc (Decl a)) vdecl_list in
     let ctxt2 = List.fold_left (fun acc stmt -> fst (typecheck_stmt acc stmt to_ret)) tc vdecls in
       let exp_ty = typecheck_exp ctxt2 exp in
         if exp_ty <> TBool then 
           type_error s "typecheck_stmt: for loop exp not a boolean"
         else
           let _, stmt_ty = typecheck_stmt ctxt2 stmt to_ret in
           if stmt_ty <> NoReturn then
             type_error s "typecheck_stmt: for loop stmt must be NoReturn"
           else 
             let _ = typecheck_block b ctxt2 to_ret NoReturn in
             (tc, NoReturn)
  | For _ -> type_error s "typecheck_stmt: invalid for loop"
  | While (exp,b) -> 
    if (typecheck_exp tc exp) <> TBool then
      type_error s "typecheck_stmt: while loop exp not a boolean"
    else
      let _ = typecheck_block b tc to_ret NoReturn in
      (tc, NoReturn)
  end
and typecheck_block (fbody:Ast.block) (tc:Tctxt.t) (ret_ty:Ast.ret_ty) (s_ty:stmt_type): Tctxt.t * stmt_type = 
  begin match fbody with
  | [] -> (tc, s_ty)
  | h::t -> 
  let new_tc, new_ty = typecheck_stmt tc h ret_ty in
    if s_ty = Return then 
      if new_ty = Return then
        type_error h "typecheck_block: early return error"
      else
        typecheck_block t tc ret_ty s_ty
    else 
      typecheck_block t new_tc ret_ty new_ty
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
      | Some fs -> () 
                     (*
        let typecheck_rty_helper (f:Ast.field) : unit =  
          begin match f.ftyp with
          | TRef (RStruct id') -> 
            if id <> id' then 
              typecheck_ty l tc f.ftyp
            else 
              ()
          | _ -> typecheck_ty l tc f.ftyp
          end in
        List.iter typecheck_rty_helper fs
                      *)
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
let typecheck_fdecl (tc : Tctxt.t) (f : Ast.fdecl) (l : 'a Ast.node)  =
  let fty = lookup_function_option f.name tc in
    begin match fty with
    | None -> raise (Invalid_argument (String.concat " " ["undefined symbol"; f.name]))
    | Some (ty_list, ret_ty) ->
      let new_tc = 
        List.fold_left (fun acc (ty, id) -> 
          begin match ty with
          | TRef (RFun fty) -> Tctxt.add_function acc id fty
          | _ -> Tctxt.add_local acc id ty
          end
        ) tc f.args in
        let _, stmt_type = typecheck_block f.body new_tc ret_ty NoReturn in
      if stmt_type = Return then 
        ()
      else 
        type_error l "typecheck_fdecl: function does not have return stmt"
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
  let helper (acc:Tctxt.t) (decl:Ast.decl): Tctxt.t = 
    begin match decl with
    | Gtdecl tnode -> 
      let id, fields = tnode.elt in
        let ty = lookup_struct_option id acc in
          if ty <> None then
            type_error tnode "create_struct_ctxt: struct duplicate"
          else
            if check_dups fields then 
              type_error tnode "create_struct_ctxt: tdecl fields contains dup"
            else
              add_struct acc id fields
    | _ -> acc
    end in
  List.fold_left helper { locals = []; globals = []; functions = []; structs = [];} p
  


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
       if lookup_global_option gnode.elt.name acc <> None then
         type_error gnode "create_global_ctxt: duplicate global vars"
       else
         add_global acc gnode.elt.name (typecheck_exp tc gnode.elt.init)
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
