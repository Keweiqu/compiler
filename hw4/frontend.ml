open Ll
open Ast

(* instruction streams ------------------------------------------------------ *)

(* As in the last project, we'll be working with a flattened representation
   of LLVMlite programs to make emitting code easier. This version
   additionally makes it possible to emit elements will be gathered up and
   "hoisted" to specific parts of the constructed CFG
   - G of gid * Ll.gdecl: allows you to output global definitions in the middle
     of the instruction stream. You will find this useful for compiling string
     literals
   - E of uid * insn: allows you to emit an instruction that will be moved up
     to the entry block of the current function. This will be useful for 
     compiling local variable declarations
*)

type elt = 
  | L of Ll.lbl             (* block labels *)
  | I of uid * Ll.insn      (* instruction *)
  | T of Ll.terminator      (* block terminators *)
  | G of gid * Ll.gdecl     (* hoisted globals (usually strings) *)
  | E of uid * Ll.insn      (* hoisted entry block instructions *)

type stream = elt list
let ( >@ ) x y = y @ x
let ( >:: ) x y = y :: x
let lift : (uid * insn) list -> stream = List.rev_map (fun (x,i) -> I (x,i))

(* Build a CFG and collection of global variable definitions from a stream *)
let cfg_of_stream (code:stream) : Ll.cfg * (Ll.gid * Ll.gdecl) list  =
    let gs, einsns, insns, term_opt, blks = List.fold_left
      (fun (gs, einsns, insns, term_opt, blks) e ->
        match e with
        | L l ->
           begin match term_opt with
           | None -> 
              if (List.length insns) = 0 then (gs, einsns, [], None, blks)
              else failwith @@ Printf.sprintf "build_cfg: block labeled %s has\
                                               no terminator" l
           | Some terminator ->
              (gs, einsns, [], None, (l, {insns; terminator})::blks)
           end
        | T t  -> (gs, einsns, [], Some t, blks)
        | I (uid,insn)  -> (gs, einsns, (uid,insn)::insns, term_opt, blks)
        | G (gid,gdecl) ->  ((gid,gdecl)::gs, einsns, insns, term_opt, blks)
        | E (uid,i) -> (gs, (uid, i)::einsns, insns, term_opt, blks)
      ) ([], [], [], None, []) code
    in
    match term_opt with
    | None -> failwith "build_cfg: entry block has no terminator" 
    | Some terminator -> 
       let insns = einsns @ insns in
       ({insns; terminator}, blks), gs


(* compilation contexts ----------------------------------------------------- *)

(* To compile OAT variables, we maintain a mapping of source identifiers to the
   corresponding LLVMlite operands. Bindings are added for global OAT variables
   and local variables that are in scope. *)

module Ctxt = struct

  type t = (Ast.id * (Ll.ty * Ll.operand)) list
  let empty = []

  (* Add a binding to the context *)
  let add (c:t) (id:id) (bnd:Ll.ty * Ll.operand) : t = (id,bnd)::c

  (* Lookup a binding in the context *)
  let lookup (id:Ast.id) (c:t) : Ll.ty * Ll.operand =
    List.assoc id c

  (* Lookup a function, fail otherwise *)
  let lookup_function (id:Ast.id) (c:t) : Ll.fty * Ll.operand =
    match List.assoc id c with
    | Fun ft, g -> ft, g
    | _ -> failwith @@ id ^ " not bound to a function"

end

(* compiling OAT types ------------------------------------------------------ *)

(* The mapping of source types onto LLVMlite is straightforward. Booleans and ints
   are represented as the the corresponding integer types. OAT strings are 
   pointers to bytes (I8). Arrays are the most interesting type: they are
   represented as pointers to structs where the first component is the number
   of elements in the following array.

   The trickiest part of this project will be satisfying LLVM's rudimentary type
   system. Recall that global arrays in LLVMlite need to be declared with their
   length in the type to statically allocate the right amount of memory. The 
   global strings and arrays you emit will therefore have a more specific type
   annotation than the output of cmp_rty. You will have to carefully bitcast
   gids to satisfy the type checker.
*)

let rec cmp_ty : Ast.ty -> Ll.ty = function
  | Ast.TVoid  -> Void
  | Ast.TBool  -> I1
  | Ast.TInt   -> I64
  | Ast.TRef r -> Ptr (cmp_rty r)
  | Ast.TFun f -> Fun (cmp_fty f)

and cmp_fty (ts,r:Ast.fty) : Ll.fty =
  List.map cmp_ty ts, cmp_ty r

and cmp_rty : Ast.rty -> Ll.ty = function
  | Ast.RString  -> I8
  | Ast.RArray u -> Struct [I64; Array(0, cmp_ty u)]

let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Eq | Neq | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)

let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)


(* Some useful helper functions *)

(* Generate a fresh temporary identifier. Since OAT identifiers cannot begin
   with an underscore, these should not clash with any source variables *)
let gensym : string -> string =
  let c = ref 0 in
  fun (s:string) -> incr c; Printf.sprintf "_%s%d" s (!c)

(* Generate code to allocate an array of source type TRef (RArray t) of the
   given size. Note "size" is an operand whose value can be computed at
   runtime *)
let oat_alloc_array (t:Ast.ty) (size:Ll.operand) : Ll.ty * operand * stream =
  let ans_id, arr_id = gensym "array", gensym "raw_array" in
  let ans_ty = cmp_ty @@ TRef (RArray t) in
  let arr_ty = cmp_ty @@ TRef (RArray TInt) in
  ans_ty, Id ans_id, lift
    [ arr_id, Call(arr_ty, Gid "oat_alloc_array", [I64, size])
    ; ans_id, Bitcast(arr_ty, Id arr_id, ans_ty) ]



(* Compile an expression exp in context c, outputting the Ll operand that will
   recieve the value of the expression, and the stream of instructions
   implementing the expression. 

   Tips:
   - use the provided cmp_ty function!

   - string literals (CStr s) should be hoisted. You'll need to bitcast the 
     resulting gid to (Ptr I8)

   - use the provided "oat_alloc_array" function to implement literal arrays
     (CArr) and the (NewArr) expressions

   - we found it useful to write a helper function 
     cmp_exp_as_ty : Ctxt.t -> Ast.exp node -> Ll.ty -> Ll.operand * stream
     that compiles an expression and optionally inserts a bitcast to the
     desired Ll type. This is useful for dealing with OAT identifiers that
     correspond to gids that don't quite have the type you want
*)

let cmp_bop (bop:Ast.binop) (ty:Ll.ty) (op1: Ll.operand) (op2: Ll.operand) (uid:string): Ll.ty * stream = 
  begin match bop with
    | Add -> (ty, [I (uid, Binop (Ll.Add, ty, op1, op2))])
    | Sub -> (ty, [I (uid, Binop (Ll.Sub, ty, op1, op2))]) 
    | Mul -> (ty, [I (uid, Binop (Ll.Mul, ty, op1, op2))])
    | Eq -> (I1, [I (uid, Icmp (Ll.Eq, ty, op1, op2))])
    | Neq -> (I1, [I (uid, Icmp (Ll.Ne, ty, op1, op2))])
    | Lt -> (I1, [I (uid, Icmp (Ll.Slt, ty, op1, op2))]) 
    | Lte -> (I1, [I (uid, Icmp (Ll.Sle, ty, op1, op2))]) 
    | Gt -> (I1, [I (uid, Icmp (Ll.Sgt, ty, op1, op2))]) 
    | Gte -> (I1, [I (uid, Icmp (Ll.Sge, ty, op1, op2))]) 
    | And -> (I1, [I (uid, Binop (Ll.And, ty, op1, op2))])
    | IAnd -> (ty, [I (uid, Binop (Ll.And, ty, op1, op2))])
    | Or -> (I1, [I (uid, Binop (Ll.Or, ty, op1, op2))])
    | IOr -> (ty, [I (uid, Binop (Ll.Or, ty, op1, op2))])
    | Shl -> (ty, [I (uid, Binop (Ll.Shl, ty, op1, op2))])
    | Shr -> (ty, [I (uid, Binop (Ll.Lshr, ty, op1, op2))]) 
    | Sar -> (ty, [I (uid, Binop (Ll.Ashr, ty, op1, op2))])
  end


let rec cmp_exp (c:Ctxt.t) (exp:Ast.exp node) : Ll.ty * Ll.operand * stream =
  begin match exp.elt with
    | CNull t -> (cmp_ty t, Ll.Null, [])
    | CBool b -> 
      begin match b with
        | true -> (cmp_ty TBool, Ll.Const 1L, [])
        | false -> (cmp_ty TBool, Ll.Const 0L, [])
      end
    | CInt n -> (cmp_ty TInt, Ll.Const n, [])
    | CStr s -> 
      let string_id = gensym "str" in
      let length = String.length s in
      (Ptr (Array (length + 1, I8)), Ll.Gid string_id, [G (string_id, (Array (length + 1, I8), Ll.GString s))])
    | CArr (ty, exp_list) ->
      let t = cmp_ty ty in
      let carr_op = gensym "carr" in
      let size_op = gensym "size" in
      let length = List.length exp_list in
      let stream1 = 
        [I (carr_op, Alloca (Struct [I64; Array (length, t)]))] >@
        [I (size_op, Gep (Ptr (Struct [I64; Array (length, t)]), Ll.Id carr_op, [Ll.Const 0L; Ll.Const 0L] ))] >@
        [I ("", Store (I64, Ll.Const (Int64.of_int length), Ll.Id size_op))] in
       let exp_list_stream, _ = 
        let carr_helper (acc:(stream * int64)) (exp: exp node) : (stream * int64) = 
          let exp_ty, exp_op, exp_stream = cmp_exp c exp in
          let gep_op = gensym "gep" in
            if exp_ty != t then
              let bitcast_op = gensym "bitcast" in
                let new_stream = (fst acc) >@
                                 exp_stream >@
                                 [I (bitcast_op, Bitcast (exp_ty, exp_op, t))] >@ 
                                 [I (gep_op, Gep (Ptr (Struct [I64; Array (length, t)]), Ll.Id carr_op, [Ll.Const 0L; Ll.Const 1L; Ll.Const (snd acc)] ))] >@
                                 [I ("", Store (t, Ll.Id bitcast_op, Ll.Id gep_op))] in
              (new_stream, Int64.add 1L (snd acc))
            else 
              let new_stream = (fst acc) >@
                               exp_stream >@
                               [I (gep_op, Gep (Ptr (Struct [I64; Array (length, t)]), Ll.Id carr_op, [Ll.Const 0L; Ll.Const 1L; Ll.Const (snd acc)] ))] >@
                               [I ("", Store (t, exp_op, Ll.Id gep_op))] in
              (new_stream, Int64.add 1L (snd acc))
         in List.fold_left carr_helper ([], 0L) exp_list in
         let bitcast_arr = gensym "bitcast_arr" in
         let new_stream = stream1 >@ exp_list_stream >@ [I (bitcast_arr, Bitcast (Ptr (Struct [I64; Array (length, t)]), Ll.Id carr_op, Ptr (Struct [I64; Array (0, t)])))]
         in (Ptr (Struct [I64; Array (0, t)]), Ll.Id bitcast_arr, new_stream)
    | NewArr (t, exp) ->
      let exp_ty, exp_op, exp_stream = cmp_exp c exp in
      let size_op, size_stream = 
        if exp_ty != I64 then
          let bitcast_op = gensym "bitcast" in
            (Ll.Id bitcast_op, exp_stream >@ [I (bitcast_op, Bitcast (exp_ty, exp_op, I64))]) 
        else 
          (exp_op, exp_stream)
      in let ty, new_op, new_stream = oat_alloc_array t size_op in
      (ty, new_op, size_stream >@ new_stream)
    | Id id -> 
      let t, operand = Ctxt.lookup id c in 
        begin match t with
          | Ll.Ptr ty -> 
              begin match ty with
                | Array (_, _)| Struct [I64; Array(_, _)] -> (t, operand, []) 
                | _ -> 
                  let id_operand = gensym "id" in
                    (ty, Ll.Id id_operand, [I (id_operand, Load (t, operand))])
              end
          | _ -> (t, operand, [])
        end
    | Index (exp1, exp2) -> 
      let ty1, op1, stream1 = cmp_exp c exp1 in 
      let ty2, op2, stream2 = cmp_exp c exp2 in
      let idx_op = gensym "idx" in
      let idx_val = gensym "idx_val" in
      begin match ty1 with
        | Ptr (Struct [size; Array (_, ty)]) ->
          (ty, Ll.Id idx_val, stream1 
                        >@ stream2 
                        >@ [I (idx_op, Gep (ty1, op1, [Ll.Const 0L; Ll.Const 1L; op2]))] 
                        >@ [I (idx_val, Load (Ptr ty, Ll.Id idx_op))])
        | Ptr Array (size, ty) ->
          (ty, Ll.Id idx_val, stream1 >@ stream2 >@ [I (idx_op, Gep (ty1, op1, [Ll.Const 0L; op2]))] >@ [I (idx_val, Load (Ptr ty, Ll.Id idx_op))])
        | _ -> raise (Invalid_argument "index into a non-array object")
      end
    | Call (id, exp_list) -> 
      let fun_ty, fname = Ctxt.lookup id c in
      begin match fun_ty with
        | Fun (args_ty, ret_ty) ->       
          let call_operand = gensym "call" in
          let args, arg_stream = cmp_args c exp_list args_ty in
          let new_stream = 
            arg_stream >@ [I (call_operand, Ll.Call (ret_ty, fname, args))] in
            (ret_ty, Ll.Id call_operand, new_stream)
        | _ -> raise (Invalid_argument "cmp_exp not a valid function type")
      end
    | Bop (bop, exp1, exp2) -> 
      let t1, op1, s1 = cmp_exp c exp1 in
      let t2, op2, s2 = cmp_exp c exp2 in
      let new_op = gensym "bop" in 
      let rt_ty, s3 = cmp_bop bop t1 op1 op2 new_op in
      (rt_ty, Ll.Id new_op, s1 >@ s2 >@ s3)
    | Uop (op, exp) ->
      let t1, op1, s1 = cmp_exp c exp in
      let new_op = gensym "uop" in
      begin match op with
        | Neg -> (cmp_ty TInt, Ll.Id new_op, s1 >@ [I (new_op, Binop (Sub, Ll.I64, Ll.Const 0L, op1))])
        | Lognot -> (cmp_ty TBool, Ll.Id new_op, s1 >@ [I (new_op, Binop (Xor, Ll.I1, Ll.Const 1L, op1))])
        | Bitnot -> (cmp_ty TInt, Ll.Id new_op, s1 >@ [I (new_op, Binop (Xor, Ll.I64, Ll.Const (Int64.neg 1L), op1))])
      end
  end

and cmp_args (c:Ctxt.t) (exp_list: exp node list) (args_ty:Ll.ty list): ((Ll.ty * Ll.operand) list * stream) =
  let cmp_args_helper (acc: ((Ll.ty * Ll.operand) list * stream)) (exp:(exp node * Ll.ty)) : ((Ll.ty * Ll.operand) list* stream) = 
    let exp_ty, exp_op, exp_stream = cmp_exp c (fst exp) in
        if exp_ty != snd exp then 
          let bitcast_op = gensym "bitcast" in
            ((fst acc) @ [(snd exp, Ll.Id bitcast_op)], (snd acc) >@ exp_stream >@ [I (bitcast_op, Bitcast (exp_ty, exp_op, snd exp))])
        else 
            ((fst acc) @ [(snd exp, exp_op)], (snd acc) >@ exp_stream)
  in List.fold_left cmp_args_helper ([], []) (List.combine exp_list args_ty)



(* Compile a statement in context c with return typ rt. Return a new context,
   possibly extended with new local bindings, and the instruction stream
   implementing the statement.

   Left-hand-sides of assignment statements must either be OAT identifiers,
   or an index into some arbitrary expression of array type. Otherwise, the
   program is not well-formed and your compiler may throw an error.

   Tips:
   - for local variable declarations, you will need to emit Allocas in the
     entry block of the current function using the E() constructor.

   - don't forget to add a bindings to the context for local variable 
     declarations
   
   - you can avoid some work by translating For loops to the corresponding
     While loop, building the AST and recursively calling cmp_stmt

   - you might find it helpful to reuse the code you wrote for the Call
     expression to implement the SCall statement

   - compiling the left-hand-side of an assignment is almost exactly like
     compiling the Id or Index expression. Instead of loading the resulting
     pointer, you just need to store to it!

 *)
let rec cmp_stmt (c:Ctxt.t) (rt:Ll.ty) (stmt:Ast.stmt node) : Ctxt.t * stream =
  begin match stmt.elt with
    | Ast.Assn (exp1, exp2) -> 
      begin match exp1.elt with
        | Id id ->
          let t1, op1 = Ctxt.lookup id c in 
          let t2, op2, stream2 = cmp_exp c exp2 in 
            if t1 != (Ptr t2) then
              let bitcast_op = gensym "bitcast" in
              begin match t1 with
                | Ptr t -> (c, stream2 >@ [I (bitcast_op, Bitcast (t2, op2, t))] >@ [I ("",Store (t, Ll.Id bitcast_op, op1))])
                | _ -> raise (Invalid_argument "stmt id ctxt not a pointer")
              end
            else
              (c, stream2 >@ [I ("", Store(t2, op2, op1))])
        | Index (e1, e2) -> 
           let ty1, op1, stream1 = cmp_exp c e1 in 
           let ty2, op2, stream2 = cmp_exp c e2 in
           let val_ty, val_op, val_stream = cmp_exp c exp2 in
           let idx_op = gensym "idx" in
           let stream3, ele_ty = 
             begin match ty1 with
             | Ptr (Struct [size; Array (_, ty)]) ->
                (val_stream >@ stream1 >@ stream2 >@ [I (idx_op, Gep (ty1, op1, [Ll.Const 0L; Ll.Const 1L; op2]))], ty)
             | Ptr Array (size, ty) ->
                (val_stream >@ stream1 >@ stream2 >@ [I (idx_op, Gep (ty1, op1, [Ll.Const 0L; op2]))], ty)
             | _ -> raise (Invalid_argument "index into a non-array object")
             end in
           let bitcast_stream = 
             if ele_ty != val_ty then
               let bitcast_op = gensym "bitcast" in
               [I (bitcast_op, Bitcast(val_ty, val_op, ele_ty))] >@ [I ("", Store (ele_ty, Ll.Id bitcast_op, Ll.Id idx_op))]
             else
               [I ("", Store (val_ty, val_op, Ll.Id idx_op))] in
           (c, stream3 >@ bitcast_stream)

        | _ -> raise (Invalid_argument "cmp_stmt invalid assignment left-hand side.")
      end
    | Ast.Decl (id, exp) -> 
      let ty, exp_op, exp_s = cmp_exp c exp in
      let uid = gensym "decl" in
      let decl_s = [E (uid, Alloca ty); I ("", Store (ty, exp_op, Ll.Id uid))] in
      let new_c = Ctxt.add c id (Ll.Ptr ty, Ll.Id uid) in 
      let stream = exp_s >@ decl_s in 
      (new_c, stream)
    | Ast.Ret None-> (c, [T (Ll.Ret (rt, None))])
    | Ast.Ret Some exp -> 
      let ty, operand, stream =  cmp_exp c exp in
        if ty != rt then
          let bitcast_op = gensym "bitcast" in
            (c, stream >@ [I (bitcast_op, Bitcast (ty, operand, rt))] >@ [T (Ll.Ret (rt, Some (Ll.Id bitcast_op)))])
        else
          (c, stream >@ [T (Ll.Ret (rt, Some operand))])
    | Ast.SCall (id, exp_list) -> 
      let fun_ty, fname = Ctxt.lookup id c in
      begin match fun_ty with
        | Fun (args_ty, ret_ty) ->       
          let args, arg_stream = cmp_args c exp_list args_ty in
          let new_stream = 
            arg_stream >@ [I ("", Ll.Call (ret_ty, fname, args))] in
          (c, new_stream)
        | _ -> raise (Invalid_argument "cmp_stmt not a valid function type")
      end
    | Ast.If (exp, b1, b2) -> 
      let _, exp_op, exp_stream = cmp_exp c exp in
      let b1_stream = cmp_block c rt b1 in 
      let b2_stream = cmp_block c rt b2 in 
      let b1_lbl = gensym "b1" in 
      let b2_lbl = gensym "b2" in
      let merge_lbl = gensym "merge" in
      let new_stream = exp_stream >@ [T (Cbr (exp_op, b1_lbl, b2_lbl))] 
          >@ [L b1_lbl] >@ b1_stream >@ [T (Br merge_lbl)] >@ [L b2_lbl] >@ b2_stream >@ [T (Br merge_lbl)] >@ [L merge_lbl] in
      (c, new_stream)
    | Ast.For (vdecl_list, None, None, b) -> 
      let decl_ctxt, decl_stream = 
        let decl_helper (acc:(Ctxt.t * stream)) (vdecl:vdecl) : (Ctxt.t * stream) = 
         let new_ctxt, new_stream = 
           let id, e = vdecl in cmp_stmt (fst acc) Ll.Void {elt = (Ast.Decl (id, e)); loc = stmt.loc}
          in (new_ctxt, (snd acc) >@ new_stream)
       in List.fold_left decl_helper (c, []) vdecl_list in
      let _, exp_op, exp_stream = cmp_exp decl_ctxt {elt = (CBool true); loc = stmt.loc} in 
      let b_stream = cmp_block decl_ctxt rt b in
      let pre_lbl = gensym "lpre" in
      let body_lbl = gensym "lbody" in
      let post_lbl = gensym "lpost" in
      let new_stream = decl_stream >@ [T (Br pre_lbl)] >@ [L pre_lbl] >@ exp_stream >@ [T (Cbr (exp_op, body_lbl, post_lbl))]
          >@ [L body_lbl] >@ b_stream >@ [T (Br pre_lbl)] >@ [L post_lbl] 
      in (c, new_stream)
    | Ast.For (vdecl_list, Some  exp, Some for_stmt, b) -> 
      let decl_ctxt, decl_stream = 
        let decl_helper (acc:(Ctxt.t * stream)) (vdecl:vdecl) : (Ctxt.t * stream) = 
         let new_ctxt, new_stream = 
           let id, e = vdecl in cmp_stmt (fst acc) Ll.Void {elt = (Ast.Decl (id, e)); loc = stmt.loc}
          in (new_ctxt, (snd acc) >@ new_stream)
       in List.fold_left decl_helper (c, []) vdecl_list in
      let _, exp_op, exp_stream = cmp_exp decl_ctxt exp in 
      let b_stream = cmp_block decl_ctxt rt b in
      let _, incr_stream = cmp_stmt decl_ctxt rt for_stmt in
      let pre_lbl = gensym "lpre" in
      let body_lbl = gensym "lbody" in
      let post_lbl = gensym "lpost" in
      let new_stream = decl_stream >@ [T (Br pre_lbl)] >@ [L pre_lbl] >@ exp_stream >@ [T (Cbr (exp_op, body_lbl, post_lbl))]
          >@ [L body_lbl] >@ b_stream >@ incr_stream >@ [T (Br pre_lbl)] >@ [L post_lbl] 
      in (c, new_stream)    
    | Ast.While (exp, b) ->
      let _, exp_op, exp_stream = cmp_exp c exp in 
      let b_stream = cmp_block c rt b in
      let pre_lbl = gensym "lpre" in
      let body_lbl = gensym "lbody" in
      let post_lbl = gensym "lpost" in
      let new_stream = [T (Br pre_lbl)] >@ [L pre_lbl] >@ exp_stream >@ [T (Cbr (exp_op, body_lbl, post_lbl))]
          >@ [L body_lbl] >@ b_stream >@ [T (Br pre_lbl)] >@ [L post_lbl] in
      (c, new_stream)
    | Ast.For (vdecl_list, None, Some _, b) | Ast.For (vdecl_list, Some _, None, b) ->
      raise (Invalid_argument "cmp_stmt for statment incorrect argument")
  end

(* Compile a series of statements *)
and cmp_block (c:Ctxt.t) (rt:Ll.ty) (stmts:Ast.block) : stream =
  snd @@ List.fold_left (fun (c, code) s -> 
      let c, stmt_code = cmp_stmt c rt s in
      c, code >@ stmt_code
    ) (c,[]) stmts


(* Populate a context with bindings for global variables and functions,
   mapping OAT identifiers to LLVMlite gids and their types.

   Only a small subset of OAT expressions can be used as global initializers
   in well-formed programs (The constructors starting with C). *)
let cmp_global_ctxt (c:Ctxt.t) (p:Ast.prog) : Ctxt.t =
  let helper (c:Ctxt.t) (d:Ast.decl) : Ctxt.t =
    begin match d with
      | Gvdecl {elt = v; _} -> 
        let global_id = v.name in
        begin match v.init.elt with
          | CInt n -> Ctxt.add c v.name (Ll.Ptr (cmp_ty Ast.TInt), Ll.Gid global_id)
          | CStr s -> 
            let length = String.length s in
            Ctxt.add c v.name (Ll.Ptr (Array(length + 1, I8)), Ll.Gid global_id)
          | CNull t -> Ctxt.add c v.name (Ll.Ptr (cmp_ty t), Ll.Gid global_id)
          | CBool b -> Ctxt.add c v.name (Ll.Ptr (cmp_ty Ast.TBool), Ll.Gid global_id)
          | CArr (t, arr) -> 
            let length = List.length arr in 
            Ctxt.add c v.name (Ll.Ptr (Struct [I64; Array (length, cmp_ty t)]), Ll.Gid global_id)
          | _ -> raise (Invalid_argument "not a global expression.")
        end
      | Gfdecl {elt = f; _} -> 
         let fty = Ast.TFun ((List.map fst f.args), f.rtyp) in
           Ctxt.add c f.name (cmp_ty fty, Ll.Gid f.name)
    end in 
   List.fold_left helper c p


(* Compile a function declaration in global context c. Return the LLVMlite cfg
   and a list of global declarations containing the string literals appearing
   in the function.

   You will need to
   1. Allocate stack space for the function parameters using Alloca
   2. Store the function arguments in their corresponding alloca'd stack slot
   3. Extend the context with bindings for function variables
   3. Compile the body of the function using cmp_block
   4. Use cfg_of_stream to produce a LLVMlite cfg from 
 *)
let cmp_fdecl (c:Ctxt.t) (f:Ast.fdecl node) : Ll.fdecl * (Ll.gid * Ll.gdecl) list =
  let param_ty_list = 
    let get_type_helper (arg: Ast.ty * Ast.id) : Ll.ty = 
      cmp_ty (fst arg) in
  List.map get_type_helper f.elt.args in

  let param_list = 
    let get_id_helper (arg: Ast.ty * Ast.id) : string = 
      snd arg in
  List.map get_id_helper f.elt.args in
  
  let uid_list = 
    let gen_uids_helper (uids: string list) (arg: Ast.ty * Ast.id) : string list = 
      uids @ [gensym (snd arg)] in
    List.fold_left gen_uids_helper [] f.elt.args in
  
  let alloca_insns = 
    let alloca_helper (uid:string) (arg: Ast.ty * Ast.id): (Ll.uid * Ll.insn) = 
      (uid, Ll.Alloca (cmp_ty (fst arg))) in
    List.map2 alloca_helper uid_list f.elt.args in 
  
  let store_insns = 
    let store_helper (uid:string) (arg: Ast.ty * Ast.id): (Ll.uid * Ll.insn) = 
      ("", Ll.Store ( cmp_ty (fst arg), Ll.Id (snd arg), Ll.Id uid)) in
    List.map2 store_helper uid_list f.elt.args in 
   
  let added_ctxt =
    let add_ctxt_helper (uid:string) (arg: Ast.ty * Ast.id) : (Ast.id * (Ll.ty * Ll.operand)) =
      (snd arg, (Ll.Ptr (cmp_ty (fst arg)), Ll.Id uid)) in
    List.map2 add_ctxt_helper uid_list f.elt.args in
  let new_ctxt = added_ctxt @ c in
  let prelude = lift (alloca_insns @ store_insns) in
  let body = cmp_block new_ctxt (cmp_ty f.elt.rtyp) f.elt.body in
  let stream = prelude >@ body in
  let cfg, g_info = cfg_of_stream stream in
  let fdecl = { fty = (param_ty_list, cmp_ty f.elt.rtyp); param = param_list; cfg = cfg } in
  (fdecl, g_info)


(* Compile a global initializer, returning the resulting LLVMlite global
   declaration, and a list of additional global declarations.

   Tips:
   - Only CNull, CBool, CInt, CStr, and CArr can appear as global initializers
     in well-formed OAT programs. Your compiler may throw an error for the other
     cases

   - OAT arrays are always handled via pointers. A global array of arrays will
     be an array of pointers to arrays emitted as additional global declarations
 *)
let rec cmp_gexp (e:Ast.exp node) : Ll.gdecl * (Ll.gid * Ll.gdecl) list =
  begin match e.elt with
    | CNull t -> ((cmp_ty t, Ll.GNull), [])  
    | CBool b -> 
      begin match b with
        | true -> ((cmp_ty Ast.TBool, Ll.GInt 1L), [])
        | false -> ((cmp_ty Ast.TBool, Ll.GInt 0L), [])
      end
    | CInt n -> ((cmp_ty Ast.TInt, Ll.GInt n), [])
    | CStr s -> 
      let length = String.length s in
      (( Array (length + 1, I8), GString s), [])
    | CArr (ty, exp_list) ->
      let gdecls, gid_gdecl_list = 
       let cmp_carr_helper (acc: (Ll.gdecl list * (Ll.gid * Ll.gdecl) list)) (exp:exp node) : (Ll.gdecl list * (Ll.gid * Ll.gdecl) list) =
          let decl, decl_list = cmp_gexp exp in 
          ((fst acc) @ [decl], decl_list @ (snd acc))
        in List.fold_left cmp_carr_helper ([], []) exp_list
      in ((Struct [I64; Array ((List.length exp_list), cmp_ty ty)], 
           GStruct [
                     (I64, GInt (Int64.of_int (List.length exp_list))); 
                     (Array((List.length exp_list), cmp_ty ty), GArray gdecls)
                   ]), 
           gid_gdecl_list)

    | _ -> raise (Invalid_argument "cmp_gexp invalid global initializer.")
  end


(* Oat initial context ------------------------------------------------------ *)
let internals =
  [ "oat_malloc",              Ll.Fun ([I64], Ptr I64)
  ; "oat_alloc_array",         Ll.Fun ([I64], Ptr (Struct [I64; Array (0, I64)]))
  ; "oat_assert_not_null",     Ll.Fun ([Ptr I8], Void)
  ; "oat_assert_array_length", Ll.Fun ([Ptr I64; I64], Void)
  ]

let builtins =
  [ "array_of_string",  cmp_ty @@ TFun ([TRef RString],  TRef(RArray TInt))
  ; "string_of_array",  cmp_ty @@ TFun ([TRef(RArray TInt)], TRef RString)
  ; "length_of_string", cmp_ty @@ TFun ([TRef RString],  TInt)
  ; "string_of_int",    cmp_ty @@ TFun ([TInt],  TRef RString)
  ; "string_cat",       cmp_ty @@ TFun ([TRef RString; TRef RString], TRef RString)
  ; "print_string",     cmp_ty @@ TFun ([TRef RString],  TVoid)
  ; "print_int",        cmp_ty @@ TFun ([TInt],  TVoid)
  ; "print_bool",       cmp_ty @@ TFun ([TBool], TVoid)
  ]

(* Compile a OAT program to LLVMlite *)
let cmp_prog (p:Ast.prog) : Ll.prog =
  (* add built-in functions to context *)
  let init_ctxt = 
    List.fold_left (fun c (i, t) -> 
        Ctxt.add c i (t, Gid i)
      ) Ctxt.empty builtins in
  (* build global variable context *)
  let c = cmp_global_ctxt init_ctxt p in
  (* compile functions and global variables *)
  let fdecls, gdecls = 
    List.fold_right (fun d (fs, gs) ->
        match d with
        | Ast.Gvdecl { elt=gd } -> 
           let ll_gd, gs' = cmp_gexp gd.init in
           (fs, (gd.name, ll_gd)::gs' @ gs)
        | Ast.Gfdecl fd ->
           let fdecl, gs' = cmp_fdecl c fd in
           (fd.elt.name,fdecl)::fs, gs' @ gs
      ) p ([], [])
  in
  (* gather external declarations *)
  let edecls = internals @ builtins in
  { tdecls=[]; gdecls; fdecls; edecls }
