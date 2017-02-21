(* ll ir compilation -------------------------------------------------------- *)

open Ll
open X86


(* allocated llvmlite function bodies --------------------------------------- *)

(* Generating X86 assembly is tricky, and it helps to split the problem into
   two parts: 

   1) Figuring out how to represent states of the LLVMlite machine as
      those of the X86lite machine, i.e. where should we store uid %x
      or global @foo, and

   2) Choosing the X86 instructions that will correspond to LLVMlite
      instructions while maintaining this relationship: For example,
      what sequence of X86 instructions will implement the
      "getelementptr" instruction, assuming that we know the arguments
      are in such and such X86 registers?

   To do this, we will introduce a slightly different representation
   of LLVMlite, where uids, globals, and labels have been replaced
   with their X86 counterparts.  Uids can be mapped onto X86
   registers, stack slots or, for instructions like "store" that do
   not assign to their uid, no storage. Additionally, since in
   LLVMlite labels and uids share a namespace, some uids correspond to
   code labels.

   Rather than working directly with the LLVMlite AST, we will be
   using a flattened "stream of instructions" representation like the
   one you saw in class. Once we have decided how we want to represent
   LLVMlite states in X86, we can convert programs to this
   representation. Then, we will have to choose X86 instructions to
   correspond to the "allocated" LLVMlite instruction stream.
*)
module Alloc = struct

(* X86 locations *)
type loc =
  | LVoid                       (* no storage *)
  | LReg of X86.reg             (* x86 register *)
  | LStk of int                 (* a stack offset from %rbp *)
  | LLbl of X86.lbl             (* an assembler label *)

type operand = 
  | Null
  | Const of int64
  | Gid of X86.lbl
  | Loc of loc

type insn =
  | ILbl
  | Binop of bop * ty * operand * operand
  | Alloca of ty
  | Load of ty * operand
  | Store of ty * operand * operand
  | Icmp of Ll.cnd * ty * operand * operand
  | Call of ty * operand * (ty * operand) list
  | Bitcast of ty * operand * ty
  | Gep of ty * operand * operand list
  | Ret of ty * operand option
  | Br of loc
  | Cbr of operand * loc * loc

(* An allocated function body is just a flattened list of instructions,
   labels, and terminators. All uids, labels, and gids are replaced with the
   associated parts of the x86 machine *)
type fbody = (loc * insn) list

(* Converting between Ll function bodies and allocate function bodies given
   two functions
   f : uid -> loc
   g : gid -> X86.lbl *)
let map_operand f g : Ll.operand -> operand = function
  | Null -> Null
  | Const i -> Const i
  | Gid x -> Gid (g x)
  | Id u -> Loc (f u)

let map_insn f g : Ll.insn -> insn = 
  let mo = map_operand f g in function
  | Binop (b,t,o,o') -> Binop (b,t,mo o,mo o')
  | Alloca t         -> Alloca t
  | Load (t,o)       -> Load (t,mo o)
  | Store (t,o,o')   -> Store (t,mo o,mo o')
  | Icmp (c,t,o,o')  -> Icmp (c,t,mo o,mo o')
  | Call (t,o,args)  -> Call (t,mo o,List.map (fun (t,o) -> t, mo o) args)
  | Bitcast (t,o,t') -> Bitcast (t,mo o,t')
  | Gep (t,o,is)     -> Gep (t,mo o,List.map mo is)

let map_terminator f g : Ll.terminator -> insn = 
  let mo = map_operand f g in function
  | Ret (t,None)   -> Ret (t, None)
  | Ret (t,Some o) -> Ret (t, Some (mo o))
  | Br l           -> Br (f l)
  | Cbr (o,l,l')   -> Cbr (mo o,f l,f l')

let of_block f g (b:Ll.block) : fbody =
  List.map (fun (u,i) -> f u, map_insn f g i) b.insns
  @ [LVoid, map_terminator f g b.terminator]
                                
let of_lbl_block f g (l,b:Ll.lbl * Ll.block) : fbody =
  (LLbl (Platform.mangle l), ILbl)::of_block f g b

let of_cfg f g (e,bs:Ll.cfg) : fbody =
  List.(flatten @@ of_block f g e :: map (of_lbl_block f g) bs)

end

(* locals and layout -------------------------------------------------------- *)

(* One key problem in compiling the LLVM IR is how to map its local
   identifiers to X86 abstractions.  For the best performance, one
   would want to use an X86 register for each LLVM %uid that is assigned
   a value.  However, since there are an unlimited number of %uids and
   only 16 registers, doing so effectively is quite difficult. We will 
   see later in the course how _register allocation_ algorithms can do a 
   good job at this.

   A simpler, but less performant, implementation is to map each %uid
   in the LLVM source to a _stack slot_ (i.e. a region of memory in
   the stack). Since LLVMlite, unlike real LLVM, permits %uid locals
   to store only 64-bit data, each stack slot is an 8-byte value.

   [ NOTE: For compiling LLVMlite, even i1 data values should be
   represented as a 8-byte quad. This greatly simplifies code
   generation. ]

   We call the datastructure that maps each %uid to its stack slot a
   'stack layout'. A stack layout maps a uid to an Alloc.loc that represents
   where it will be stored. Recall that some uids identify instructions that
   do not assign a value, whereas others name code blocks. These are mapped to 
   Alloc.LVoid, and Alloc.LLbl, respectively. For this compilation strategy, 
   uids that are assigned values will always be assigned an offset from ebp
   (in bytes) that corresponds to a storage slot in the stack.  
*)
type layout = (uid * Alloc.loc) list 


(* Once we have a layout, it's simple to generate the allocated version of our
   LLVMlite program *)
let alloc_cfg (layout:layout) (g:Ll.cfg) : Alloc.fbody =
  Alloc.of_cfg (fun x -> List.assoc x layout) 
               (fun l -> Platform.mangle l) g

(* streams of x86 instructions ---------------------------------------------- *)

type x86elt = 
  | I of X86.ins
  | L of (X86.lbl * bool)

type x86stream = x86elt list 

let lift : X86.ins list -> x86stream =
  List.rev_map (fun i -> I i)

let ( >@ ) x y = y @ x
let ( >:: ) x y = y :: x

let prog_of_x86stream : x86stream -> X86.prog =
  let rec loop p iis = function
    | [] -> (match iis with [] -> p | _ -> failwith "stream has no initial label")
    | (I i)::s' -> loop p (i::iis) s'
    | (L (l,global))::s' -> loop ({ lbl=l; global; asm=Text iis }::p) [] s'
  in loop [] []

(* compiling operands  ------------------------------------------------------ *)

(* LLVM IR instructions support several kinds of operands.

   LL local %uids live in stack slots, whereas global ids live at
   global addresses that must be computed from a label.  Constants are
   immediately available, and the operand Null is the 64-bit 0 value.

   You might find it useful to implement the following helper function, 
   whose job is to generate the X86 operand corresponding to an allocated 
   LLVMlite operand.
 *)
let compile_operand (operand:Alloc.operand) : X86.operand = 
  begin match operand with
    | Alloc.Null -> Imm (Lit 0L)
    | Alloc.Const c -> Imm (Lit c)
    | Alloc.Gid lbl -> Imm (Lbl (Platform.mangle lbl))
    | Alloc.Loc loc -> 
      begin match loc with
        | Alloc.LVoid -> raise (Invalid_argument "operand is LVoid")
        | Alloc.LReg reg -> Ind2 reg
        | Alloc.LStk x -> Ind3 (Lit (Int64.of_int x), Rbp)
        | Alloc.LLbl lbl -> Imm (Lbl (Platform.mangle lbl))
      end
  end

let compile_binop (loc:Alloc.loc) (bop, ty, operand1, operand2: (bop * ty * Alloc.operand * Alloc.operand)) : x86stream =
  let move_operands = [
                        I (Movq, [(compile_operand operand1); Reg R11]);
                        I (Movq, [(compile_operand operand2); Reg Rcx]);
                      ] in
  let perform_binop =  begin match bop with
    | Ll.Add -> [I (Addq, [Reg Rcx; Reg R11])]
    | Ll.Sub -> [I (Subq, [Reg Rcx; Reg R11])]
    | Ll.Mul -> [I (Imulq, [Reg Rcx; Reg R11])]
    | Ll.Shl -> [I (Shlq, [Reg Rcx; Reg R11])]
    | Ll.Lshr -> [I (Shrq, [Reg Rcx; Reg R11])]
    | Ll.Ashr -> [I (Sarq, [Reg Rcx; Reg R11])]
    | Ll.And -> [I (Andq, [Reg Rcx; Reg R11])]
    | Ll.Or -> [I (Orq, [Reg Rcx; Reg R11])]
    | Ll.Xor -> [I (Xorq, [Reg Rcx; Reg R11])]
  end in
  let move_to_loc = 
    begin match loc with
      | Alloc.LVoid -> []
      | Alloc.LReg reg -> [I (Movq, [Reg R11; Reg reg])] 
      | Alloc.LStk x -> [I (Movq, [Reg R11; Ind3 (Lit (Int64.of_int x), Rbp)])]
      | Alloc.LLbl lbl -> [I (Movq, [Reg R11; Imm (Lbl lbl)])]
    end in
  move_to_loc @ perform_binop @ move_operands

let compile_br (loc:Alloc.loc) : x86stream = 
  Printf.printf "in compile_br\n";
  begin match loc with
    | Alloc.LLbl s -> Printf.printf "%s\n" s
    | _ -> ()
  end;
  [I (Jmp, [compile_operand (Alloc.Loc loc)])]

let map_cnd (cnd:Ll.cnd): X86.cnd = 
  begin match cnd with
    | Ll.Eq -> X86.Eq
    | Ll.Ne -> X86.Neq
    | Ll.Slt -> X86.Lt
    | Ll.Sle -> X86.Le
    | Ll.Sgt -> X86.Gt
    | Ll.Sge -> X86.Ge
  end

let compile_icmp (loc:Alloc.loc) ((c, t, op1, op2):(Ll.cnd * ty * Alloc.operand * Alloc.operand)) : x86stream = 
  [
    I (Set (map_cnd c), [compile_operand (Alloc.Loc loc)]);
    I (Cmpq, [compile_operand op2; Reg R11]);
    I (Movq, [compile_operand op1; Reg R11]);
    I (Movq, [Imm (Lit 0L); compile_operand (Alloc.Loc loc)])
  ]

let compile_cbr ((operand, loc1, loc2): (Alloc.operand * Alloc.loc * Alloc.loc)) : x86stream = 
  [
    I (J Neq, [compile_operand (Alloc.Loc loc1)]);
    I (J Eq, [compile_operand (Alloc.Loc loc2)]);
    I (Cmpq, [Imm (Lit 0L); (compile_operand operand)])
  ]


(* compiling call  ---------------------------------------------------------- *)

(* You will probably find it helpful to implement a helper function that 
   generates code for the LLVM IR call instruction.

   The code you generate should follow the x64 System V AMD64 ABI
   calling conventions, which places the first six 64-bit (or smaller)
   values in registers and pushes the rest onto the stack.  Note that,
   since all LLVM IR operands are 64-bit values, the first six
   operands will always be placed in registers.  (See the notes about
   compiling fdecl below.)

   [ NOTE: It is the caller's responsibility to clean up arguments
   pushed onto the stack, so you must free the stack space after the
   call returns. ]

   [ NOTE: Don't forget to preserve caller-save registers (only if
   needed). ]
*)


let caller_save_regs (cur_86stream: x86stream) : x86stream = 
  (I (Pushq, [Reg Rcx])) ::
  (I (Pushq, [Reg Rdx])) ::
  (I (Pushq, [Reg Rsi])) :: 
  (I (Pushq, [Reg Rdi])) :: 
  (I (Pushq, [Reg R08])) :: 
  (I (Pushq, [Reg R09])) :: 
  (I (Pushq, [Reg R10])) ::
  (I (Pushq, [Reg R11])) :: 
  cur_86stream

let rec put_args (os: (ty * Alloc.operand) list) (idx: int) (cur_86stream: x86stream) : x86stream =
  begin match os with
    | [] -> cur_86stream
    | (_, op)::tail ->
      begin match idx with
        | 0 -> put_args tail (idx + 1) 
                        (I (Movq, [compile_operand op; Reg Rdi])::cur_86stream)
        | 1 -> put_args tail (idx + 1) 
                        (I (Movq, [compile_operand op; Reg Rsi])::cur_86stream)
        | 2 -> put_args tail (idx + 1) 
                        (I (Movq, [compile_operand op; Reg Rdx])::cur_86stream)
        | 3 -> put_args tail (idx + 1) 
                        (I (Movq, [compile_operand op; Reg Rcx])::cur_86stream)
        | 4 -> put_args tail (idx + 1) 
                        (I (Movq, [compile_operand op; Reg R08])::cur_86stream)
        | 5 -> put_args tail (idx + 1) 
                        (I (Movq, [compile_operand op; Reg R09])::cur_86stream)
        | x -> put_args tail (idx + 1) 
                        (I (Pushq, [compile_operand op])::cur_86stream)
      end
  end 

let clean_restore_caller_regs (arg_length: int) (cur_86stream:x86stream) : x86stream =
  (I (Movq, [Ind3 (Lit (-64L), Rsp);Reg Rcx])) ::
  (I (Movq, [Ind3 (Lit (-56L), Rsp);Reg Rdx])) ::
  (I (Movq, [Ind3 (Lit (-48L), Rsp);Reg Rsi])) :: 
  (I (Movq, [Ind3 (Lit (-40L), Rsp);Reg Rdi])) :: 
  (I (Movq, [Ind3 (Lit (-32L), Rsp);Reg R08])) :: 
  (I (Movq, [Ind3 (Lit (-24L), Rsp);Reg R09])) :: 
  (I (Movq, [Ind3 (Lit (-16L), Rsp);Reg R10])) ::
  (I (Movq, [Ind3 (Lit (-8L), Rsp); Reg R11])) ::
  (I (Addq, [Imm (Lit (Int64.of_int ((arg_length - 6 + 8) * 8))); Reg Rsp])) ::
  cur_86stream

let compile_call (fo:Alloc.operand) (os:(ty * Alloc.operand) list) : x86stream = 
  let caller_save_stream = caller_save_regs [] in
  let save_args_stream = put_args os 0 caller_save_stream in
  let call_callee = (I (Callq, [compile_operand fo])) :: save_args_stream in
    clean_restore_caller_regs (List.length os) call_callee


let compile_return (loc: Alloc.loc) (insn: Alloc.insn) (cur_stream:x86stream) : x86stream = 
  let restore_regs_stream = 
    (I (Movq, [ Ind3 (Lit (-8L), Rbp); Reg Rbx ])) ::
    (I (Movq, [ Ind3 (Lit (-16L), Rbp); Reg R12 ])) ::
    (I (Movq, [ Ind3 (Lit (-24L), Rbp); Reg R13 ])) ::
    (I (Movq, [ Ind3 (Lit (-32L), Rbp); Reg R14 ])) ::
    (I (Movq, [ Ind3 (Lit (-40L), Rbp); Reg R15 ])) ::
    cur_stream in
  let put_to_rax_stream = 
    begin match insn with
      | Alloc.Ret (_, None) -> restore_regs_stream
      | Alloc.Ret (_, Some operand) -> (I (Movq, [compile_operand operand; Reg Rax])) :: restore_regs_stream
      | _ -> raise (Invalid_argument "Not a Ret insn")
    end in
  let restore_rbp_rsp_stream = 
    (I (Movq, [Ind2 Rbp; Reg Rbp])) ::
    (I (Addq, [Imm (Lit 8L); Reg Rsp])) ::
    (I (Movq, [Reg Rbp; Reg Rsp])) ::
    put_to_rax_stream in
      (I (Retq, [])) :: restore_rbp_rsp_stream

    

(* compiling getelementptr (gep)  ------------------------------------------- *)

(* The getelementptr instruction computes an address by indexing into
   a datastructure, following a path of offsets.  It computes the
   address based on the size of the data, which is dictated by the
   data's type.

   To compile getelmentptr, you must generate x86 code that performs
   the appropriate arithemetic calculations.
*)

(* [size_ty] maps an LLVMlite type to a size in bytes. 
    (needed for getelementptr)

   - the size of a struct is the sum of the sizes of each component
   - the size of an array of t's with n elements is n * the size of t
   - all pointers, I1, and I64 are 8 bytes
   - the size of a named type is the size of its definition

   - Void, i8, and functions have undefined sizes according to LLVMlite.
     Your function should simply return 0 in those cases
*)

let rec find_type_alias tdecls (tid:string) : ty = 
  begin match tdecls with
    | [] -> raise (Invalid_argument "tid undefined")
    | (t, ty)::tail ->
      if t = tid then
        ty
      else 
        find_type_alias tail tid
  end

let rec size_ty tdecls t : int =
  begin match t with
    | Void | I8 | Fun _ -> 0
    | I1 | I64 | Ptr _ -> 8
    | Struct t_list -> 
      let helper (acc:int) (t:ty) : int =
        acc + (size_ty tdecls t)
      in List.fold_left helper 0 t_list
    | Array (size, elem) -> size * (size_ty tdecls elem) 
    | Namedt tid -> size_ty tdecls (find_type_alias tdecls tid)
  end


let compile_alloca (loc:Alloc.loc) tdecls (t:ty) : x86stream =
  [
    I (Addq, [Imm (Lit (Int64.of_int (size_ty tdecls t))); Reg Rsp]);
    I (Movq, [Reg Rsp; compile_operand (Alloc.Loc loc)])
  ]


let rec compile_store tdecls ((t, op1, op2): (ty * Alloc.operand * Alloc.operand)) : x86stream = 
  begin match t with
    | Void | I8 | Fun _ | Struct _ | Array _ -> raise (Invalid_argument "invalid store operand")
    | I1 | I64 | Ptr _ -> 
      [
        I (Movq, [Reg R11; compile_operand op2]);
        I (Movq, [compile_operand op1; Reg R11])
      ]
    | Namedt tid -> 
      let new_t = find_type_alias tdecls tid in
        compile_store tdecls (new_t, op1, op2)
  end

let rec compile_load tdecls (loc:Alloc.loc) ((t, op):(ty * Alloc.operand)) : x86stream =
  let x86_op = compile_operand op in
  let load_op = 
    begin match x86_op with
      | Imm x -> Ind1 x
      | Reg r -> Ind2 r
      | x -> x
    end in
  begin match t with
    | Void | I8 | Fun _ | Struct _ | Array _ -> raise (Invalid_argument "invalid load operand")
    | I1 | I64 | Ptr _ -> 
      [
        I (Movq, [Reg R11; compile_operand (Alloc.Loc loc)]);
        I (Movq, [load_op; Reg R11])
      ]
    | Namedt tid -> 
      let new_t = find_type_alias tdecls tid in
        compile_load tdecls loc (new_t, op)
  end

(* Generates code that computes a pointer value.  

   1. o must be a pointer of type t=*t'

   2. the value of o is the base address of the calculation

   3. the first index in the path is treated as the index into an array
     of elements of type t' located at the base address

   4. subsequent indices are interpreted according to the type t':

     - if t' is a struct, the index must be a constant n and it 
       picks out the n'th element of the struct. [ NOTE: the offset
       within the struct of the n'th element is determined by the 
       sizes of the types of the previous elements ]

     - if t' is an array, the index can be any operand, and its
       value determines the offset within the array.
 
     - if t' is any other type, the path is invalid

     - make sure you can handle named types!

   5. if the index is valid, the remainder of the path is computed as
      in (4), but relative to the type f the sub-element picked out
      by the path so far
*)

let compile_getelementptr tdecls (t:Ll.ty) 
                          (o:Alloc.operand) (os:Alloc.operand list) : x86stream =
  if os = [] then
      [I (Movq, [compile_operand o; Reg R11])]     
  else 
    let h::tail = os in 
    let init_offset = 
      begin match h with
        | Const x -> (size_ty tdecls t) * (Int64.to_int x)
        | _ -> raise (Invalid_argument "gep op2-opn must be int64")
      end in
    let rec helper (acc:int) tdecls (t:Ll.ty) (os: Alloc.operand list) : int =
      begin match os with
        | [] -> acc
        | h::tail -> 
          begin match h with
            | Const x -> 
              begin match t with 
                | Struct -> helper (struct_helper + acc) new_type tail
                | Array
                | Void | I8 | Fun _ -> raise (Invalid_argument "struct has invalid field")
                | I1 | I64 | Ptr _ -> 8
              end
            | _ -> raise (Invalid_argument "must be int64")
          end
      end
      


(* compiling instructions within function bodies ---------------------------- *)

(* An Alloc.fbody value is a list of LLVM lite labels, instructions,
   and terminators.  The compile_fbody function can process each of these
   in sequence, generating a corresponding stream of x86 instructions.

   The result of compiling a single LLVM instruction might be many x86
   instructions.  We have not determined the structure of this code
   for you. Some of the instructions require only a couple assembly
   instructions, while others require more.  We have suggested that
   you need at least compile_operand, compile_call, and compile_gep
   helpers; you may introduce more as you see fit.

   Here are a few tips:

   - The goal of this project is _not_ to produce efficient code. Emit
     extra moves liberally, using Rax and Rcx as scratch registers.
     You should aim for correctness first, making sure you don't
     violate restrictions of x86-64 assembly (e.g. the number of
     memory operands allowed for an instruction!)

   - The type of x86streams and their operations make appending to a
     stream efficient. You might find it useful to define a tail-
     recursive helper function that passes an output stream as an
     accumulator.

   - Icmp:  the Set instruction may be of use.  Depending on how you
     compile Cbr, you may want to ensure that the value produced by
     Icmp is exactly 0 or 1.

   - Load & Store: these need to dereference the pointers. Const and
     Null operands aren't valid pointers.  Don't forget to
     Platform.mangle the global identifier.

   - Alloca: needs to return a pointer into the stack

   - Bitcast: does nothing interesting at the assembly level

   Compiling block terminators is not too difficult:

   - Ret should properly exit the function: freeing stack space,
     restoring the value of %rbp, and putting the return value (if
     any) in %rax.

   - Br should jump

   - Cbr branch should treat its operand as a boolean conditional
*)

let compile_fbody tdecls (af:Alloc.fbody) : x86stream =
  let rec step_helper (acc: x86stream) (ins: (Alloc.loc * Alloc.insn)) : x86stream =
    let loc, insn = ins in
      begin match insn with
        | Alloc.ILbl -> 
          begin match loc with
            | Alloc.LLbl lbl -> (L (lbl, false)) :: acc
            | _ -> raise (Failure "not a label")
          end
        | Alloc.Binop (b, t, op1, op2) -> (compile_binop loc (b, t, op1, op2)) @ acc
        | Alloc.Alloca t -> (compile_alloca loc tdecls t) @ acc
        | Alloc.Store (t, op1, op2) -> (compile_store tdecls (t, op1, op2)) @ acc
        | Alloc.Load (t, op) -> (compile_load tdecls loc (t, op)) @ acc
        | Alloc.Ret _ -> compile_return loc insn acc
        | Alloc.Br loc1 -> (compile_br loc1) @ acc
        | Alloc.Icmp (c, t, op1, op2) -> (compile_icmp loc (c, t, op1, op2)) @ acc
        | Alloc.Call (t, fo, os) ->
          begin match t with
            | Ll.Void -> (compile_call fo os) @ acc
            | I1 | I64 | Ptr _ -> ((I (Movq, [Reg Rax; compile_operand (Alloc.Loc loc)])) :: (compile_call fo os)) @ acc
            | Struct _ | Array _ | Fun _ | I8 -> raise (Invalid_argument "invalid call arg")
            | Namedt nt -> 
              let new_t = find_type_alias tdecls nt in
                step_helper acc (loc, Alloc.Call (new_t, fo, os))
          end
        | Alloc.Bitcast (t1, op, t2) -> 
         [ I (Movq, [Reg R11; compile_operand (Alloc.Loc loc)]);
           I (Movq, [compile_operand op; Reg R11])] @
         acc
        | Alloc.Gep (t, op, os) -> (compile_getelementptr tdecls t op os) @ acc
        | Alloc.Cbr (operand, loc1, loc2) -> (compile_cbr (operand, loc1, loc2)) @ acc
      end in
  List.fold_left step_helper [] af

(* compile_fdecl ------------------------------------------------------------ *)

(* We suggest that you create a helper function that computes the 
   layout for a given function declaration.

   - each function argument should be copied into a stack slot
   - in this (inefficient) compilation strategy, each local id 
     is also stored as a stack slot.
   - uids associated with instructions that do not assign a value,
     such as Store and a Call of a Void function should be associated
     with Alloc.LVoid
   - LLVMlite uids and labels share a namespace. Block labels you encounter
     should be associated with Alloc.Llbl
*)

let rec block_layout_helper (offset:int) (acc:layout) (insns:(uid * Ll.insn) list) : (layout * int) =
  begin match insns with
    | [] -> (acc, offset)
    | (uid, insn)::t -> 
      begin match insn with
        | Store _ | Call (Void, _, _) -> block_layout_helper offset ((uid, Alloc.LVoid) :: acc) t
        | _ -> block_layout_helper (offset - 8) ((uid, Alloc.LStk offset) :: acc) t
      end
  end

let rec lbl_block_layout_helper (offset:int) (acc:layout) (blocks: (lbl * block) list) : (layout * int) =
  begin match blocks with
    | [] -> (acc, offset)
    | (lbl, block)::t -> 
      let new_layout, new_offset = block_layout_helper offset acc block.insns in
      lbl_block_layout_helper new_offset ((lbl, Alloc.LLbl lbl) :: new_layout) t
  end

let cfg_layout_helper (offset:int) (acc:layout) ((blck, blcks):Ll.cfg) : (layout * int) = 
  let new_layout, new_offset = block_layout_helper offset acc blck.insns in
  lbl_block_layout_helper new_offset new_layout blcks
  

let rec param_layout_helper (offset:int) (acc:layout) (params:uid list) : (layout * int) = 
  begin match params with
    | [] -> (acc, offset)
    | h::t -> param_layout_helper (offset - 8) ((h, Alloc.LStk offset) :: acc) t
  end

let stack_layout (f:Ll.fdecl) : (layout * int) =
  let layout, offset = param_layout_helper (-48) [] f.param in
    cfg_layout_helper offset layout f.cfg


(* The code for the entry-point of a function must do several things:

   - since our simple compiler maps local %uids to stack slots,
     compiling the control-flow-graph body of an fdecl requires us to
     compute the layout (see the discussion of locals and layout). Use
     the provided alloc_cfg function to produce an allocated function
     body.

   - the function code should also comply with the calling
     conventions, typically by moving arguments out of the parameter
     registers (or stack slots) into local storage space.  For our
     simple compilation strategy, that local storage space should be
     in the stack. (So the function parameters can also be accounted
     for in the layout.)

   - the function entry code should allocate the stack storage needed
     to hold all of the local stack slots.
*)

(* This helper function computes the location of the nth incoming
   function argument: either in a register or relative to %rbp,
   according to the calling conventions.  You might find it useful for
   compile_fdecl.

   [ NOTE: the first six arguments are numbered 0 .. 5 ]
*)
let arg_loc (n : int) : X86.operand =
  begin match n with
    | 0 -> Reg Rdi
    | 1 -> Reg Rsi
    | 2 -> Reg Rdx
    | 3 -> Reg Rcx
    | 4 -> Reg R08
    | 5 -> Reg R09
    | x -> Ind3 (Lit (Int64.of_int ((x - 4) * 8)), Rbp)
  end  

let rec lookup_layout (uid:uid) (stack_layout:layout) : Alloc.loc = 
  begin match stack_layout with
    | [] -> raise (Failure "Did not find uid in stack layout")
    | (u, loc)::t -> 
      if uid = u then
        loc
      else
        lookup_layout uid t
  end

let rec copy_parameter (params:uid list) (stack_layout:layout) (cur_stream:x86stream) (idx:int) : x86stream = 
  begin match params with
    | [] -> cur_stream
    | uid::t ->   
      let caller_loc = arg_loc idx in
        begin match caller_loc with
          | Reg reg -> copy_parameter t stack_layout ((I (Pushq, [Reg reg])):: cur_stream) (idx + 1)
          | Ind3 (x, Rbp) -> copy_parameter t stack_layout ((I (Pushq, [Ind3 (x, Rbp)])) :: cur_stream) (idx + 1)
          | _ -> raise (Failure "cannot copy parameter")
        end  
  end

let callee_save_regs (cur_86stream: x86stream) : x86stream = 
  (I (Pushq, [Reg R15])) ::
  (I (Pushq, [Reg R14])) ::
  (I (Pushq, [Reg R13])) ::
  (I (Pushq, [Reg R12])) :: 
  (I (Pushq, [Reg Rbx])) :: 
  cur_86stream

let rec print_layout (layout:layout) : unit = 
  begin match layout with
    | [] -> ()
    | (uid, _)::t -> Printf.printf "%s\n" uid; print_layout t
  end

(* To do: 1. Save Caller, Callee registers, possibly change arg_loc *)
let compile_fdecl tdecls (g:gid) (f:Ll.fdecl) : x86stream =
  let init_stack_frame_stream = 
    if g = "main" then
      [I (Movq, [Reg Rsp; Reg Rbp]); I (Pushq, [Reg Rbp]); I (Movq, [Reg Rsp; Reg Rbp]); L ((Platform.mangle g), true)]
    else
      [I (Movq, [Reg Rsp; Reg Rbp]); I (Pushq, [Reg Rbp]); L ((Platform.mangle g),true)]
  in
  let callee_save_stream = callee_save_regs init_stack_frame_stream in
  let layout, offset = stack_layout f in
  let copy_param_stream =  copy_parameter f.param layout callee_save_stream 0 in
  let fbody = alloc_cfg layout f.cfg in
  let body_stream = compile_fbody tdecls fbody in
  (*
    Printf.printf "------------ stack layout ------------------\n";
    print_layout layout;
    Printf.printf "--------------------------------------------\n"; 
   *)
    body_stream @ 
    [I (Addq, [Imm (Lit (Int64.of_int offset)); Reg Rsp])] @
    [I (Movq, [Reg Rbp; Reg Rsp])] @
    copy_param_stream
   

  

(* compile_gdecl ------------------------------------------------------------ *)

(* Compile a global value into an X86 global data declaration and map
   a global uid to its associated X86 label.
*)
let rec compile_ginit = function
  | GNull      -> [Quad (Lit 0L)]
  | GGid gid   -> [Quad (Lbl (Platform.mangle gid))]
  | GInt c     -> [Quad (Lit c)]
  | GString s  -> [Asciz s]
  | GArray gs 
  | GStruct gs -> List.map compile_gdecl gs |> List.flatten

and compile_gdecl (_, g) = compile_ginit g

(* compile_prog ------------------------------------------------------------- *)

let compile_prog {tdecls; gdecls; fdecls} : X86.prog =
  let g = fun (lbl, gdecl) -> Asm.data (Platform.mangle lbl) (compile_gdecl gdecl) in
  let f = fun (name, fdecl) -> prog_of_x86stream @@ compile_fdecl tdecls name fdecl in
  (List.map g gdecls) @ (List.map f fdecls |> List.flatten)
