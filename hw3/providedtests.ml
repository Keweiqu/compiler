open Assert
open X86
open Ll
open Backend

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)



let exec_e2e_ast ll_ast args extra_files =
  let output_path = !Platform.output_path in
  let dot_s_file = Platform.gen_name output_path "test" ".s" in
  let exec_file = Platform.gen_name output_path "exec" "" in
  let asm_ast = Backend.compile_prog ll_ast in
  let asm_str = X86.string_of_prog asm_ast in
  let _ = Driver.write_file dot_s_file asm_str in
  let _ = Platform.link (dot_s_file::extra_files) exec_file in
  let result = Driver.run_executable args exec_file in
  (* let _ = Platform.sh (Printf.sprintf "rm -f %s %s" dot_s_file exec_file) Platform.ignore_error in *)
  let _ = Platform.verb @@ Printf.sprintf "** Executable exited with: %d\n" result in
  Int64.of_int result
  

let exec_e2e_file path args =
  let ast = Driver.parse_file path in
  exec_e2e_ast ast args []

let executed tests =
  List.map (fun (fn, ans) ->
      fn, assert_eqf (fun () -> exec_e2e_file fn "") ans)
    tests

let gcd_tests = 
  [ "llprograms/gcd1071_462.ll", 21L
  ; "llprograms/gcd4_2.ll", 2L
  ]

let provided_tests : suite = [
  GradedTest("gcd tests", 2, executed gcd_tests)
] 
