(* X86lite Simulator *)

(* See the documentation in the X86lite specification, available on the 
   course web pages, for a detailed explanation of the instruction
   semantics.
*)

open X86
(* simulator machine state -------------------------------------------------- *)

let mem_bot = 0x400000L          (* lowest valid address *)
let mem_top = 0x410000L          (* one past the last byte in memory *)
let mem_size = Int64.to_int (Int64.sub mem_top mem_bot)
let nregs = 17                   (* including Rip *)
let ins_size = 4L                (* assume we have a 4-byte encoding *)
let exit_addr = 0xfdeadL         (* halt when m.regs(%rip) = exit_addr *)

(* Your simulator should raise this exception if it tries to read from or
   store to an address not within the valid address space. *)
exception X86lite_segfault

(* The simulator memory maps addresses to symbolic bytes.  Symbolic
   bytes are either actual data indicated by the Byte constructor or
   'symbolic instructions' that take up four bytes for the purposes of
   layout.

   The symbolic bytes abstract away from the details of how
   instructions are represented in memory.  Each instruction takes
   exactly four consecutive bytes, where the first byte InsB0 stores
   the actual instruction, and the next three bytes are InsFrag
   elements, which aren't valid data.

   For example, the two-instruction sequence:
        at&t syntax             ocaml syntax
      movq %rdi, (%rsp)       Movq,  [~%Rdi; Ind2 Rsp]
      decq %rdi               Decq,  [~%Rdi]

   is represented by the following elements of the mem array (starting
   at address 0x400000):

       0x400000 :  InsB0 (Movq,  [~%Rdi; Ind2 Rsp])
       0x400001 :  InsFrag
       0x400002 :  InsFrag
       0x400003 :  InsFrag
       0x400004 :  InsB0 (Decq,  [~%Rdi])
       0x400005 :  InsFrag
       0x400006 :  InsFrag
       0x400007 :  InsFrag
*)
type sbyte = InsB0 of ins       (* 1st byte of an instruction *)
           | InsFrag            (* 2nd, 3rd, or 4th byte of an instruction *)
           | Byte of char       (* non-instruction byte *)

(* memory maps addresses to symbolic bytes *)
type mem = sbyte array

(* Flags for condition codes *)
type flags = { mutable fo : bool
             ; mutable fs : bool
             ; mutable fz : bool
             }

(* Register files *)
type regs = int64 array

(* Complete machine state *)
type mach = { flags : flags
            ; regs : regs
            ; mem : mem
            }

(* simulator helper functions ----------------------------------------------- *)

(* The index of a register in the regs array *)
let rind : reg -> int = function
  | Rip -> 16
  | Rax -> 0  | Rbx -> 1  | Rcx -> 2  | Rdx -> 3
  | Rsi -> 4  | Rdi -> 5  | Rbp -> 6  | Rsp -> 7
  | R08 -> 8  | R09 -> 9  | R10 -> 10 | R11 -> 11
  | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15

(* Helper functions for reading/writing sbytes *)

(* Convert an int64 to its sbyte representation *)
let sbytes_of_int64 (i:int64) : sbyte list =
  let open Char in 
  let open Int64 in
  List.map (fun n -> Byte (shift_right i n |> logand 0xffL |> to_int |> chr))
           [0; 8; 16; 24; 32; 40; 48; 56]

(* Convert an sbyte representation to an int64 *)
let int64_of_sbytes (bs:sbyte list) : int64 =
  let open Char in
  let open Int64 in
  let f b i = match b with
    | Byte c -> logor (shift_left i 8) (c |> code |> of_int)
    | _ -> 0L
  in
  List.fold_right f bs 0L

(* Convert a string to its sbyte representation *)
let sbytes_of_string (s:string) : sbyte list =
  let rec loop acc = function
    | i when i < 0 -> acc
    | i -> loop (Byte s.[i]::acc) (pred i)
  in
  loop [Byte '\x00'] @@ String.length s - 1

(* Serialize an instruction to sbytes *)
let sbytes_of_ins (op, args:ins) : sbyte list =
  let check = function
    | Imm (Lbl _) | Ind1 (Lbl _) | Ind3 (Lbl _, _) -> 
      invalid_arg "sbytes_of_ins: tried to serialize a label!"
    | o -> ()
  in
  List.iter check args;
  [InsB0 (op, args); InsFrag; InsFrag; InsFrag]

(* Serialize a data element to sbytes *)
let sbytes_of_data : data -> sbyte list = function
  | Quad (Lit i) -> sbytes_of_int64 i
  | Asciz s -> sbytes_of_string s
  | Quad (Lbl _) -> invalid_arg "sbytes_of_data: tried to serialize a label!"


(* It might be useful to toggle printing of intermediate states of your 
   simulator. *)
let debug_simulator = ref false

(* Interpret a condition code with respect to the given flags. *)
let interp_cnd {fo; fs; fz} : cnd -> bool = fun x -> 
  begin match x with
    | Eq -> fz
    | Neq -> not fz
    | Gt -> (fs = fo) && (not fz)
    | Ge -> fs = fo
    | Lt -> fs <> fo 
    | Le -> (fs <> fo) || fz
  end



(* Maps an X86lite address into Some OCaml array index,
   or None if the address is not within the legal address space. *)
let map_addr (addr:quad) : int option =
  if addr > mem_top || addr < mem_bot then 
    None
  else 
    Some (Int64.to_int (Int64.sub addr mem_bot))

(* Loads a int64 from memory location start from start_loc to start_loc+8 *)
let load_val_from_mem (start_loc:int64) (m:mach): int64 =
  let mem_idx = map_addr start_loc in
    begin match mem_idx with
      | None -> raise (Failure "load from mem failure") (* X86lite_segfault *)
      | Some i -> int64_of_sbytes (Array.to_list (Array.sub m.mem i 8))
    end

(* Interprets address specified by the operand *)
let interpret_operand_addr (operand: operand) (m: mach): int64 = 
  begin match operand with
    | Imm (Lit x) -> x
    | Imm (Lbl _) | Ind1 (Lbl _) | Ind3 ((Lbl _), _) -> raise (Invalid_argument "should have resolved all lables")
    | Reg reg -> m.regs.(rind reg)
    | Ind1 (Lit x) -> x
    | Ind2 reg -> m.regs.(rind reg)
    | Ind3 ((Lit x), reg) -> Int64.add m.regs.(rind reg) x
  end

(* Interprets value specified by the operand*)
let interpret_operand_val (operand: operand) (m: mach): int64 = 
  begin match operand with
    | Imm (Lit x) -> x
    | Imm (Lbl _) | Ind1 (Lbl _) | Ind3 ((Lbl _), _) -> raise (Invalid_argument "should have resolved all lables")
    | Reg reg -> m.regs.(rind reg)
    | Ind1 (Lit x) -> load_val_from_mem x m
    | Ind2 reg -> load_val_from_mem m.regs.(rind reg) m
    | Ind3 ((Lit x), reg) -> load_val_from_mem (Int64.add m.regs.(rind reg) x) m  
  end 

(* Writes an int64 to memory from  mem_loc to mem_loc + 8 *)
let write_to_mem (mem_loc:int64) (v:int64) (m:mach): unit = 
  let mem_idx = map_addr mem_loc in
    begin match mem_idx with
      | None -> raise (Failure "write to mem failure")(*X86lite_segfault*)
      | Some i -> 
        let sbyte_list = sbytes_of_int64 v in
          Array.blit (Array.of_list sbyte_list) 0 m.mem i (List.length sbyte_list)
    end

(* Updates destination specified by dest with value v *)
let update_dest (v:int64) (dest:operand) (m:mach) : unit = 
  begin match dest with
    | Imm _ | Ind1 (Lbl _) | Ind3((Lbl _), _) -> raise (Invalid_argument "dest should be a memory location not label")
    | Reg reg -> m.regs.(rind reg) <- v
    | Ind1 (Lit x) -> write_to_mem x v m
    | Ind2 reg -> 
      let mem_loc = m.regs.(rind reg) in write_to_mem mem_loc v m
    | Ind3 ((Lit x), reg) ->
      let mem_loc = Int64.add m.regs.(rind reg) x in write_to_mem mem_loc v m
  end 

(* Fetches the instruction in memory location stored in Rip *)
let fetch_ins (m:mach) : sbyte =
  let mem_idx = map_addr m.regs.(rind Rip) in
    begin match mem_idx with
      | None -> raise (Failure "fetch_ins_failure")(*X86lite_segfault *)
      | Some i -> m.mem.(i)
    end
  
(* Finishes arithmetic step with two operands *)
let binary_op_step (op:opcode) (operands:operand list) (m:mach) : unit =
  let open Int64_overflow in
  begin match operands with
    | [] | _::[] | _::_::_::_ -> raise (Invalid_argument "binary operator") 
    | a::b::[] -> 
      let {value =v; overflow = fo} =
        begin match op with
          | Addq -> add (interpret_operand_val a m) (interpret_operand_val b m)
          | Imulq -> mul (interpret_operand_val a m) (interpret_operand_val b m)
          | Subq -> Printf.printf "subq, rax=%Ld" m.regs.(rind Rax); sub (interpret_operand_val b m) (interpret_operand_val a m)
          | _ -> raise (Failure "Not valid binary operation")
        end
      in 
        begin match op with
          | Subq ->         
            if (interpret_operand_val a m) = Int64.min_int ||
            (Int64.shift_right_logical (interpret_operand_val b m) 63) = 
            (Int64.shift_right_logical (Int64.neg (interpret_operand_val a m)) 63) && 
            (Int64.shift_right_logical v 63) <> 
            (Int64.shift_right_logical (Int64.neg (interpret_operand_val a m)) 63) then
              m.flags.fo <- true
            else 
              m.flags.fo <- false
          | Addq -> 
              if (Int64.shift_right_logical (interpret_operand_val b m) 63) = 
              (Int64.shift_right_logical (interpret_operand_val a m) 63) && 
              (Int64.shift_right_logical v 63) <> 
              (Int64.shift_right_logical (interpret_operand_val a m) 63) then
                m.flags.fo <- true
              else 
                m.flags.fo <- false
          | Imulq -> m.flags.fo <- fo
          | _ -> ()
        end;
        update_dest v b m;
        m.flags.fz <-
          if v = Int64.zero then
            true
          else
            false;
        m.flags.fs <-
          if Int64.shift_right_logical v 63 = 1L then
            true
          else
            false;
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 4L
   end

(* Finishes arithmetic step with one operand *)
let unary_op_step (op:opcode) (operands:operand list) (m:mach) : unit =
  let open Int64_overflow in
  begin match operands with
    | [] | _::_::_ -> raise (Invalid_argument "unary operator") 
    | a::[] -> 
      let {value =v; overflow = fo} =
        begin match op with
          | Incq -> Printf.printf "incq, rax=%Ld\n" m.regs.(rind Rax); succ (interpret_operand_val a m)
          | Decq -> pred (interpret_operand_val a m)
          | Negq -> neg (interpret_operand_val a m)
          | _ -> raise (Failure "Not valid unary operation")
        end
      in 
        begin match op with
          | Decq ->         
            if (Int64.shift_right_logical (interpret_operand_val a m) 63) = 
            (Int64.shift_right_logical (Int64.neg 1L) 63) && 
            (Int64.shift_right_logical v 63) <> 
            (Int64.shift_right_logical (Int64.neg 1L) 63) then
              m.flags.fo <- true 
            else 
              m.flags.fo <- false
          | Incq -> 
              if (((Int64.shift_right_logical (interpret_operand_val a m) 63) = 
              (Int64.shift_right_logical 1L 63)) && 
              ((Int64.shift_right_logical v 63) <> 
              (Int64.shift_right_logical 1L 63))) then
                m.flags.fo <- true
              else 
                m.flags.fo <- false
          | Negq -> 
            if v = Int64.min_int then
              m.flags.fo <- true
            else 
               m.flags.fo <- false
          | _ -> ()
        end;      
        update_dest v a m;
        m.flags.fz <-
          if v = Int64.zero then
            true
          else
            false;
        m.flags.fs <-
          if Int64.shift_right_logical v 63 = 1L then
            true
          else
            false;
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 4L
  end

let binary_log_step (op:opcode) (operands:operand list) (m:mach) : unit =
  begin match operands with
    | [] | _::[] | _::_::_::_ -> raise (Invalid_argument "logical binary operator") 
    | a::b::[] -> 
      let v =
        begin match op with
          | Xorq -> Int64.logxor (interpret_operand_val b m) (interpret_operand_val a m)
          | Orq -> Int64.logor (interpret_operand_val b m) (interpret_operand_val a m)
          | Andq -> Int64.logand (interpret_operand_val b m) (interpret_operand_val a m)
          | _ -> raise (Failure "Not valid logical binary operation")
        end
      in 
        update_dest v b m;
        m.flags.fo <- false;
        m.flags.fz <-
          if v = Int64.zero then
            true
          else
            false;
        m.flags.fs <-
          if Int64.shift_right_logical v 63 = 1L then
            true
          else
            false;
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 4L
   end

let unary_log_step (op:opcode) (operands:operand list) (m:mach) : unit =
  begin match operands with
    | [] | _::_::_ -> raise (Invalid_argument "logical unary operator") 
    | a::[] -> 
      let v =
        begin match op with
          | Notq -> Int64.lognot (interpret_operand_val a m)
          | _ -> raise (Failure "Not valid logical unary operation")
        end
      in 
        update_dest v a m;
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 4L
   end

let bit_op_step (op:opcode) (operands:operand list) (m:mach) : unit =
  begin match operands with
    | [] | _::[] | _::_::_::_ -> raise (Invalid_argument "bitwise binary operator") 
    | a::b::[] -> 
      let amt = 
        begin match a with
          | Imm (Lit x) -> x
          | Reg Rcx -> m.regs.(rind Rcx)
          | _ -> raise (Invalid_argument "bitwise operation only takes Imm or rcx as AMT")
        end 
      in 
        let v = 
          begin match op with
            | Shlq -> Int64.shift_left (interpret_operand_val b m) (Int64.to_int amt)
            | Sarq -> Int64.shift_right (interpret_operand_val b m) (Int64.to_int amt)
            | Shrq -> Int64.shift_right_logical (interpret_operand_val b m) (Int64.to_int amt)
            | _ -> raise (Failure "Not a valid bitwise binary operation")
          end in
        let original_b = (interpret_operand_val b m) in
          update_dest v b m;
          if (Int64.to_int (interpret_operand_val a m)) <> 0 then
              begin match op with
                | Sarq ->
                  if amt = 1L then
                    m.flags.fo <- false
                | Shlq ->
                  if amt = 1L then
                   if (Int64.shift_right_logical v 63) <> (Int64.shift_right_logical original_b 63) then
                     m.flags.fo <- true;
                | Shrq ->
                  if amt = 1L then
                    if (Int64.shift_right_logical original_b 63) = 1L then
                      m.flags.fo <- true
                    else
                      m.flags.fo <- false
                | _ -> raise (Failure "Not a valid bitwise binary operation")
              end;
              m.flags.fz <-
                if v = Int64.zero then
                  true
                else
                 false;
              m.flags.fs <-
                if Int64.shift_right_logical v 63 = 1L then
                  true
                else
                  false;
          m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 4L        
  end

let move_step (operands: operand list) (m:mach) : unit = 
  Printf.printf "In movq";
  begin match operands with
    | [] | _::[] | _::_::_::_ -> raise (Invalid_argument "binary operator") 
    | a::b::[] -> 
      let v = interpret_operand_val a m in update_dest v b m
  end;
  m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 4L

let push_pop_step (op:opcode) (operands: operand list) (m:mach) : unit =
  begin match operands with
    | [] | _::_::_ -> raise (Invalid_argument "unary operator")
    | a::[] -> 
      begin match op with
        | Pushq -> 
          m.regs.(rind Rsp) <- Int64.sub m.regs.(rind Rsp) 8L;
          write_to_mem m.regs.(rind Rsp) (interpret_operand_val a m) m
        | Popq ->
          let v = load_val_from_mem m.regs.(rind Rsp) m in
            update_dest v a m;
            m.regs.(rind Rsp) <- Int64.add m.regs.(rind Rsp) 8L
        | _ -> raise (Failure "Not push or pop operator")
      end
  end    

let lea_step (operands: operand list) (m:mach) : unit = 
  begin match operands with 
    | [] | _::[] | _::_::_::_ -> raise (Invalid_argument "leaq is a binary operator")
    | a::b::[] ->
      begin match a with
        | Imm _ | Reg _ -> raise (Failure "leaq must take an Ind operand")
        | Ind1 (Lbl _) | Ind3 ((Lbl _), _) -> raise (Failure "Should have resolved all labels")
        | Ind1 (Lit x) -> update_dest x b m
        | Ind2 reg -> update_dest (m.regs.(rind reg)) b m
        | Ind3 ((Lit x), reg) -> update_dest (Int64.add (m.regs.(rind reg)) x) b m
      end
  end


let jmp_step (operands:operand list) (m:mach) : unit = 
  begin match operands with
    | [] | _::_::_ -> raise (Invalid_argument "jmp is a unary operator")
    | a::[] -> update_dest (interpret_operand_val a m) (Reg Rip) m
  end

let j_cc_step (cc:cnd) (operands:operand list) (m:mach) : unit = 
  begin match operands with
    | [] | _::_::_ -> raise (Invalid_argument "j cc is a unary operator")
    | a::[] -> 
      if (interp_cnd m.flags cc) then
        jmp_step operands m
      else
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 4L
  end

let call_step (operands: operand list) (m:mach) : unit =
  begin match operands with
    | [] | _::_::_ -> raise (Invalid_argument "jmp is a unary operator")
    | a::[] -> 
      m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 4L;
      push_pop_step Pushq [Reg Rip] m;
      m.regs.(rind Rip) <- interpret_operand_val a m
  end


let cmp_step (operands: operand list) (m:mach) : unit =
  Printf.printf "In cmpq, Rdi= %Ld\n" m.regs.(rind Rdi);
  let open Int64_overflow in
  begin match operands with 
    | [] | _::[] | _::_::_::_ -> raise (Invalid_argument "cmp is a binary operator")
    | a::b::[] ->
      let {value = v; overflow = fo} =
        sub (interpret_operand_val b m) (interpret_operand_val a m)
      in 
        if (interpret_operand_val a m) = Int64.min_int ||
        (Int64.shift_right_logical (interpret_operand_val b m) 63) = 
        (Int64.shift_right_logical (Int64.neg (interpret_operand_val a m)) 63) && 
        (Int64.shift_right_logical v 63) <> 
        (Int64.shift_right_logical (Int64.neg (interpret_operand_val a m)) 63) then
          m.flags.fo <- true
        else 
          m.flags.fo <- false;
        m.flags.fz <-
          if v = Int64.zero then
            true
          else
            false;
        m.flags.fs <-
          if Int64.shift_right_logical v 63 = 1L then
            true
          else
            false;
        m.regs.(rind Rip) <- Int64.add m.regs.(rind Rip) 4L
  end

let set_step (cc:cnd) (operands: operand list) (m:mach) : unit = 
  begin match operands with
    | [] | _::_::_ -> raise (Invalid_argument "set is a unary operator")
    | a::[] ->
      let mask = Int64.shift_right_logical (-1L) 8 in
      let v = 
        if interp_cnd m.flags cc then
          Int64.add (Int64.logand (interpret_operand_val a m) mask) 1L
        else
          Int64.logand (interpret_operand_val a m) mask
        in update_dest v a m
  end


let ret_step (operands:operand list) (m:mach) : unit = 
  begin match operands with 
    | [] -> push_pop_step Popq [Reg Rip] m
    | _ -> raise (Invalid_argument "ret does not accept operands")
  end


(* Simulates one step of the machine:
    - fetch the instruction at %rip
    - compute the source and/or destination information from the operands
    - simulate the instruction semantics
    - update the registers and/or memory appropriately
    - set the condition flags
*)
let step (m:mach) : unit =
  let open Int64_overflow in 
    let insn = fetch_ins m in
      begin match insn with
        | InsFrag -> raise (Invalid_argument "insfrag not an instruction")
        | Byte _ -> Printf.printf "rax=%Ld, rbx=%Ld" m.regs.(rind Rax) m.regs.(rind Rbx); raise (Invalid_argument "byte not an instruction")
        | InsB0 (opcode, operands) ->
          begin match opcode with
            | Movq -> move_step operands m 
            | Pushq | Popq -> push_pop_step opcode operands m
            | Leaq -> lea_step operands m 
            | Jmp -> jmp_step operands m
            | J cnd -> j_cc_step cnd operands m
            | Cmpq -> cmp_step operands m
            | Set cnd -> set_step cnd operands m
            | Callq -> call_step operands m 
            | Retq -> ret_step operands m
            | Addq | Subq | Imulq -> binary_op_step opcode operands m 
            | Incq | Decq | Negq -> unary_op_step opcode operands m
            | Xorq | Orq | Andq -> binary_log_step opcode operands m
            | Notq -> unary_log_step opcode operands m
            | Shlq | Sarq | Shrq -> bit_op_step opcode operands m
          end
      end 



(* Runs the machine until the rip register reaches a designated
   memory address. *)
let run (m:mach) : int64 = 
  while m.regs.(rind Rip) <> exit_addr do step m done;
  m.regs.(rind Rax)

(* assembling and linking --------------------------------------------------- *)

(* A representation of the executable *)
type exec = { entry    : quad              (* address of the entry point *)
            ; text_pos : quad              (* starting address of the code *)
            ; data_pos : quad              (* starting address of the data *)
            ; text_seg : sbyte list        (* contents of the text segment *)
            ; data_seg : sbyte list        (* contents of the data segment *)
            }

(* Assemble should raise this when a label is used but not defined *)
exception Undefined_sym of lbl

(* Assemble should raise this when a label is defined more than once *)
exception Redefined_sym of lbl

(* Convert an X86 program into an object file:
   - separate the text and data segments
   - compute the size of each segment
      Note: the size of an Asciz string section is (1 + the string length)

   - resolve the labels to concrete addresses and 'patch' the instructions to 
     replace Lbl values with the corresponding Imm values.

   - the text segment starts at the lowest address
   - the data segment starts after the text segment

  HINT: List.fold_left and List.fold_right are your friends.
 *)

let rec lookup (x:string) (c:(string * int64) list) : int64 =
  begin match c with
    | [] -> raise (Undefined_sym x)
    | (h::tl) ->
      if (fst h) = x then 
        snd h
      else 
        lookup x tl
  end

(* Separate text and data segments *)
let separate (p:prog) (acc:(elem list * elem list)): (elem list * elem list) =
  let assign (acc:(elem list * elem list)) (e:elem): (elem list * elem list) =
    begin match e.asm with
     | Text _ -> ((fst acc) @ [e], snd acc)
     | Data _ -> (fst acc, (snd acc) @ [e])
    end
  in
    List.fold_left assign acc p

let compute_size_of_data_elem (e:elem) : int64 =
  let size (acc:int64) (d:data) : int64 =
    begin match d with
      | Asciz s -> Int64.add (Int64.add (Int64.of_int (String.length s)) 1L) acc
      | Quad _ -> Int64.add 8L acc
    end
  in
  begin match e.asm with
   | Text _ -> raise (Invalid_argument "text element")
   | Data ds -> 
     begin match ds with
       | [] -> 0L 
       | a -> List.fold_left size 0L ds 
     end
  end

type label_helper = (int64 * ((string * int64) list))

let resolve_labels (segments:(elem list * elem list)) : (string * int64) list = 
  let all_segments = (fst segments) @ (snd segments) in 
  let compute_mem ((curr, symbol_table):label_helper) (e:elem) : label_helper =
    let incr =
      begin match e.asm with
        | Text a -> Int64.mul (Int64.of_int (List.length a)) 4L 
        | Data _ -> compute_size_of_data_elem e
      end 
     in
       try 
         let _ = lookup e.lbl symbol_table in 
           raise (Redefined_sym e.lbl)
       with 
         Undefined_sym _ -> (Int64.add curr incr, (e.lbl, curr)::symbol_table) 

   in
     let (mem, symbol_table) = 
       List.fold_left compute_mem (mem_bot, []) all_segments in 
         symbol_table
     

let text_seg_to_sbytes (text_inss: ins list) : sbyte list = 
  let serialize (text_sb: sbyte list) (ins:ins): sbyte list =
    text_sb @ (sbytes_of_ins ins) in
    List.fold_left serialize [] text_inss



let replace_operands (operands: operand list) (sym_tbl: (string * int64) list) : operand list =
  let resolve_lbl (operands: operand list) (operand:operand) : operand list =
    begin match operand with
      | Imm (Lbl l) -> operands @ [Imm (Lit (lookup l sym_tbl))]
      | Ind1 (Lbl l) -> operands @ [Ind1 (Lit (lookup l sym_tbl))]
      | Ind3 ((Lbl l), reg) -> operands @ [Ind3 (Lit (lookup l sym_tbl), reg)] 
      | _ -> operands @ [operand]
    end in
  List.fold_left resolve_lbl [] operands

let replace_ins (inss:ins list) (sym_tbl: (string * int64) list) : ins list = 
  let helper (acc: ins list) (ins:ins) : ins list = 
    let (opcode, operands) = ins in
      acc @ [(opcode, replace_operands operands sym_tbl)] in
  List.fold_left helper [] inss

let replace_data (ds: data list) (sym_tbl: (string * int64) list): data list =
  let helper (acc: data list) (data: data) : data list = 
    begin match data with 
      | Asciz _ | Quad (Lit _) -> acc @ [data]
      | Quad (Lbl l) -> acc @ [Quad (Lit (lookup l sym_tbl))]
    end in
  List.fold_left helper [] ds
 
let replace_elem_list (el:elem list) (sym_tbl: (string * int64) list) : elem list =
  let helper (acc: elem list) (e:elem) : elem list = 
    begin match e.asm with
      | Data ds -> 
        let new_ele = {e with asm = Data (replace_data ds sym_tbl)} in
          acc @ [new_ele]
      | Text ts ->
        let new_ele = {e with asm = Text (replace_ins ts sym_tbl)} in
          acc @ [new_ele]
    end in
  List.fold_left helper [] el


let replace_prog (segs: (elem list * elem list)) (sym_tbl: (string * int64) list) : (elem list * elem list) =
  (replace_elem_list (fst segs) sym_tbl, replace_elem_list (snd segs) sym_tbl) 
  
let serialize_ins (inss: ins list): sbyte list =
  let helper (acc:sbyte list) (ins:ins) : sbyte list =
    acc @ (sbytes_of_ins ins) in 
  List.fold_left helper [] inss

let serialize_dt (ds: data list) : sbyte list =
  let helper (acc:sbyte list) (dt:data) : sbyte list =
    acc @ (sbytes_of_data dt) in
  List.fold_left helper [] ds

let serialize_segs (seg: elem list) : sbyte list = 
  let helper (acc:sbyte list) (e:elem) : sbyte list = 
    begin match e.asm with
      | Data d -> acc @ (serialize_dt d)
      | Text t -> acc @ (serialize_ins t)
    end in
  List.fold_left helper [] seg



let assemble (p:prog) : exec =
  let seg_tuple = separate p ([],[]) in
  let symbol_table = resolve_labels seg_tuple in 
  let entry = lookup "main" symbol_table in
  let new_seg_tuple = replace_prog seg_tuple symbol_table in
  let text_sbytes = serialize_segs (fst new_seg_tuple) in
  let data_sbytes = serialize_segs (snd new_seg_tuple) in
  let data_pos = Int64.add mem_bot (Int64.of_int (List.length text_sbytes)) in
    { entry = entry;
      text_pos = mem_bot;
      data_pos = data_pos;
      text_seg = text_sbytes;
      data_seg = data_sbytes
    }  



(* Convert an object file into an executable machine state. 
    - allocate the mem array
    - set up the memory state by writing the symbolic bytes to the 
      appropriate locations 
    - create the inital register state
      - initialize rip to the entry point address
      - initializes rsp to the last word in memory 
      - the other registers are initialized to 0
    - the condition code flags start as 'false'

  Hint: The Array.make, Array.blit, and Array.of_list library functions 
  may be of use.
*)
let load {entry; text_pos; data_pos; text_seg; data_seg} : mach = 
  let mem = (Array.make mem_size (Byte '\x00')) in
  Array.blit (Array.of_list text_seg) 0 mem 0 (List.length text_seg);
  Array.blit (Array.of_list data_seg) 0 mem (Int64.to_int (Int64.sub data_pos mem_bot)) (List.length data_seg);
  Array.blit (Array.of_list (sbytes_of_int64 exit_addr)) 0 mem (mem_size - 8) 8;
  let regs = Array.make nregs 0L in
  regs.(rind Rip) <- entry;
  regs.(rind Rsp) <- Int64.sub mem_top 8L;
  { flags = {fo = false; fs = false; fz = false};
    regs = regs;
    mem = mem
  }
