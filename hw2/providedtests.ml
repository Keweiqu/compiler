open Assert
open X86
open Simulator
open Asm

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)
let gcd a b = [ text "mod"
                         [ Subq, [~%R08; ~%R09]
                         ; Cmpq, [~%R08; ~%R09]
                         ; J Ge, [~$$"mod"] 
                         ; Movq, [~%R09; ~%R08]
                         ; Retq, []
                         ]
                  ; text "swap"
                         [ Movq, [~%R08; ~%R10]
                         ; Movq, [~%R09; ~%R08]
                         ; Movq, [~%R10; ~%R09]
                         ; Retq, []
                         ]
                  ; text "set_min_first"
                         [ Cmpq, [~%R08; ~%R09]
                         ; J Le, [~$$"swap"]
                         ; Retq, []
                         ]
                  ; text "gcd"
                         [ Callq, [~$$"set_min_first"]
                         ; Movq, [~%R08; ~%R10]
                         ; Callq, [~$$"mod"]
                         ; Movq, [~%R10; ~%R09]
                         ; Cmpq, [~%R08; ~$0]
                         ; J Eq, [~$$"exit"]
                         ; Callq, [~$$"gcd"]
                         ; Retq, []
                         ]
                  ; text "exit"
                         [ Movq,  [~%R09; ~%Rax]
                         ; Retq, []
                         ]
                  ; gtext "main"
                         [ Movq, [~$a; ~%R08]
                         ; Movq, [~$b; ~%R09]
                         ; Callq, [~$$"gcd"]
                         ; Retq, [] 
                         ]
                  ]

let program_test (p:prog) (ans:int64) () =
  let res = assemble p |> load |> run in
  if res <> ans
  then failwith (Printf.sprintf("Expected %Ld but got %Ld") ans res)
  else ()


let provided_tests : suite = [
  Test ("Student-Provided Big Test for Part III: Score recorded as PartIIITestCase", [
  	 ("gcd1", program_test (gcd 8 9) 1L);
     ("gcd1", program_test (gcd 10 10) 10L);
     ("gcd1", program_test (gcd 121 11) 11L);
     ("gcd1", program_test (gcd  12 9) 3L);
  ]);

] 

