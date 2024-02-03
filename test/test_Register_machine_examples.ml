open ComputationTheory.Natural
open ComputationTheory.Register_machine
open ComputationTheory.Register_machine_examples

let tiny_natural_arbitrary = QCheck.make ~print:(fun n -> string_of_int (int_of_nat n)) (QCheck.Gen.map (nat_min (nat_of_int 10)) nat_gen)

let test_first_project =
  QCheck.Test.make ~count:100 ~name:"First Project"
  (QCheck.pair tiny_natural_arbitrary tiny_natural_arbitrary)
  ( fun (x,y) ->
    let out = configuration_run first_projection (instruction_zero, build_memory [nat_zero; x; y]) in
    out register_zero = x )

let test_const =
  QCheck.Test.make ~count:100 ~name:"Const"
  (QCheck.pair tiny_natural_arbitrary tiny_natural_arbitrary)
  ( fun (n,x) ->
    let out = configuration_run (const (int_of_nat n)) (instruction_zero, build_memory [nat_zero; x]) in
    out register_zero = n )

let test_truncated_subtraction =
  QCheck.Test.make ~count:100 ~name:"Truncated Subtraction"
  (QCheck.pair tiny_natural_arbitrary tiny_natural_arbitrary)
  ( fun (x,y) ->
    let out = configuration_run truncated_subtraction (instruction_zero, build_memory [nat_zero; x; y]) in
    out register_zero = nat_monus x y )

let test_integer_division =
  QCheck.Test.make ~count:100 ~name:"Integer Division"
  (QCheck.pair tiny_natural_arbitrary tiny_natural_arbitrary)
  ( fun (x,y) ->
    let out = configuration_run integer_division (instruction_zero, build_memory [nat_zero; x; y]) in
    out register_zero = if y = nat_zero then nat_zero else nat_div x y )

let test_modulo =
  QCheck.Test.make ~count:100 ~name:"Modulo"
  (QCheck.pair tiny_natural_arbitrary tiny_natural_arbitrary)
  ( fun (x,y) ->
    let out = configuration_run modulo (instruction_zero, build_memory [nat_zero; x; y]) in
    out register_zero = if y = nat_zero then x else nat_mod x y )

let suite = List.map QCheck_alcotest.to_alcotest
  [ test_first_project
  ; test_const
  ; test_truncated_subtraction
  ; test_integer_division
  ; test_modulo ]

let () =
  let open Alcotest in
  run "Natural"
  [ "", suite ]
