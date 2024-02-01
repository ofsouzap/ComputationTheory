open ComputationTheory.Natural
open ComputationTheory.Register_machine

let const a _ = a

(* Configuration stepping *)

let conf_step_test =
  QCheck.Test.make ~count:1000
  QCheck.(pair program_arbitrary configuration_arbitrary)
  ( fun (prog, conf) -> const true (configuration_step prog conf) )

let conf_step_suite = List.map QCheck_alcotest.to_alcotest
  [ conf_step_test ]

(* Adding program *)

let prog_add : program = build_program
  [ Dec (1, 1, 2)
  ; Inc (0, 0)
  ; Dec (2, 3, 4)
  ; Inc (0, 2)
  ; Halt ]

let mem_add a b c = build_memory [ nat_of_int a; nat_of_int b; nat_of_int c ]

let rm_add_init_config a b c = (instruction_zero, mem_add a b c)

let rm_add_test =
  QCheck.Test.make ~count:1000
  QCheck.(triple small_nat small_nat small_nat)
  ( fun (a, b, c) -> memory_read_output (configuration_run prog_add (rm_add_init_config a b c)) = nat_of_int (a + b + c) )

let rm_add_suite = List.map QCheck_alcotest.to_alcotest
  [ rm_add_test ]

let () =
  let open Alcotest in
  run "Register Machine"
    [ "Configuration stepping", conf_step_suite
    ; "Add machine", rm_add_suite ]
