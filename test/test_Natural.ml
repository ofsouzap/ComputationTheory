open ComputationTheory.Natural

let rec basic_int_pow a = function
  | 0 -> 1
  | 1 -> a
  | b -> if b < 0
    then failwith "Negative power"
    else a * basic_int_pow a (b-1)

let tiny_nat_arbitrary : int QCheck.arbitrary = QCheck.make @@ QCheck.Gen.int_range 0 10

let test_int_nat_int_identity =
  QCheck.Test.make ~count:1000
  QCheck.int
  ( fun (x' : int) -> let x = abs x' in int_of_nat (nat_of_int x) = x )

let test_nat_int_nat_identity =
  QCheck.Test.make ~count:1000
  nat_arbitrary
  ( fun (n : natural) -> nat_of_int (int_of_nat n) = n )

let identity_suite = List.map QCheck_alcotest.to_alcotest
  [ test_int_nat_int_identity
  ; test_nat_int_nat_identity ]

let test_succ =
  QCheck.Test.make ~count:1000
  nat_arbitrary
  ( fun (n : natural) -> int_of_nat (nat_succ n) = int_of_nat n + 1 )

let test_plus =
  QCheck.Test.make ~count:1000
  QCheck.(pair int int)
  ( fun (x',y') -> let x,y = abs x', abs y' in int_of_nat (nat_plus (nat_of_int x) (nat_of_int y)) = x + y )

let test_times =
  QCheck.Test.make ~count:1000
  QCheck.(pair int int)
  ( fun (x',y') -> let x,y = abs x', abs y' in int_of_nat (nat_times (nat_of_int x) (nat_of_int y)) = x * y )

let test_pow =
  QCheck.Test.make ~count:1000
  (QCheck.pair tiny_nat_arbitrary tiny_nat_arbitrary)
  ( fun (x',y') -> let x,y = abs x', abs y' in int_of_nat (nat_pow (nat_of_int x) (nat_of_int y)) = basic_int_pow x y )

let increase_suite = List.map QCheck_alcotest.to_alcotest
  [ test_succ
  ; test_plus
  ; test_times
  ; test_pow ]

let test_pred_or_zero =
  QCheck.Test.make ~count:1000
  nat_arbitrary
  ( fun (n : natural) -> (match int_of_nat n with
    | 0 -> nat_pred_or_zero n = nat_of_int 0
    | n_int -> nat_pred_or_zero n = nat_of_int (n_int - 1) ))

let test_monus =
  QCheck.Test.make ~count:1000
  QCheck.(pair int int)
  ( fun (x',y') ->
    let x,y = abs x', abs y' in
    let exp = if x < y then 0 else x - y in
    int_of_nat (nat_monus (nat_of_int x) (nat_of_int y)) = exp )

let decrease_suite = List.map QCheck_alcotest.to_alcotest
  [ test_pred_or_zero
  ; test_monus ]

let () =
  let open Alcotest in
  run "Natural"
  [ "Identity Conversions", identity_suite
  ; "Increasing values", increase_suite
  ; "Decreasing values", decrease_suite ]
