open ComputationTheory.Natural

let tiny_int_arbitrary = QCheck.int_bound 20

let test_int_nat_int_identity =
  QCheck.Test.make ~count:1000
  QCheck.int
  ( fun (x' : int) -> let x = Z.of_int (abs x') in z_of_nat (nat_of_z x) = x )

let test_nat_int_nat_identity =
  QCheck.Test.make ~count:1000
  nat_arbitrary
  ( fun (n : natural) -> nat_of_z (z_of_nat n) = n )

let identity_suite = List.map QCheck_alcotest.to_alcotest
  [ test_int_nat_int_identity
  ; test_nat_int_nat_identity ]

let test_succ =
  QCheck.Test.make ~count:1000
  nat_arbitrary
  ( fun (n : natural) -> z_of_nat (nat_succ n) = Z.add (z_of_nat n) Z.one )

let test_plus =
  QCheck.Test.make ~count:1000
  QCheck.(pair int int)
  ( fun (x',y') -> let x,y = Z.of_int (abs x'), Z.of_int (abs y') in z_of_nat (nat_plus (nat_of_z x) (nat_of_z y)) = Z.add x y )

let test_times =
  QCheck.Test.make ~count:1000
  QCheck.(pair int int)
  ( fun (x',y') -> let x,y = Z.of_int (abs x'), Z.of_int (abs y') in z_of_nat (nat_times (nat_of_z x) (nat_of_z y)) = Z.mul x y )

let test_pow =
  QCheck.Test.make ~count:1000
  QCheck.(pair tiny_int_arbitrary tiny_int_arbitrary)
  ( fun (x',y') -> let x,y,y_int = Z.of_int (abs x'), Z.of_int (abs y'), abs y' in
    if x' = y' && x' = 0
    then true
    else z_of_nat (nat_pow (nat_of_z x) (nat_of_z y)) = Z.pow x y_int )

let increase_suite = List.map QCheck_alcotest.to_alcotest
  [ test_succ
  ; test_plus
  ; test_times
  ; test_pow
   ]

let test_pred_or_zero =
  QCheck.Test.make ~count:1000
  nat_arbitrary
  ( fun (n : natural) -> if z_of_nat n = Z.zero
      then nat_pred_or_zero n = nat_of_z Z.zero
      else nat_pred_or_zero n = nat_of_z (Z.sub (z_of_nat n) Z.one) )

let test_monus =
  QCheck.Test.make ~count:1000
  QCheck.(pair int int)
  ( fun (x',y') ->
    let x,y = Z.of_int (abs x'), Z.of_int (abs y') in
    let exp = if Z.lt x y then Z.zero else Z.sub x y in
    z_of_nat (nat_monus (nat_of_z x) (nat_of_z y)) = exp )

let test_mod =
  QCheck.Test.make ~count:1000
  QCheck.(pair int int)
  ( fun (x',y') ->
    let x,y = Z.of_int (abs x'), Z.of_int (abs y') in
    z_of_nat (nat_mod (nat_of_z x) (nat_of_z y)) = Z.(mod) x y )

let test_div =
  QCheck.Test.make ~count:1000
  QCheck.(pair int int)
  ( fun (x',y') ->
    let x,y = Z.of_int (abs x'), Z.of_int (abs y') in
    z_of_nat (nat_div (nat_of_z x) (nat_of_z y)) = Z.div x y )

let decrease_suite = List.map QCheck_alcotest.to_alcotest
  [ test_pred_or_zero
  ; test_monus
  ; test_mod
  ; test_div ]

let test_min =
  QCheck.Test.make ~count:1000
  QCheck.(pair int int)
  ( fun (x',y') ->
    let x,y = Z.of_int (abs x'), Z.of_int (abs y') in
    z_of_nat (nat_min (nat_of_z x) (nat_of_z y)) = Z.min x y )

let test_max =
  QCheck.Test.make ~count:1000
  QCheck.(pair int int)
  ( fun (x',y') ->
    let x,y = Z.of_int (abs x'), Z.of_int (abs y') in
    z_of_nat (nat_max (nat_of_z x) (nat_of_z y)) = Z.max x y )

let compare_suite = List.map QCheck_alcotest.to_alcotest
  [ test_min
  ; test_max ]

let () =
  let open Alcotest in
  run "Natural"
  [ "Identity Conversions", identity_suite
  ; "Increasing values", increase_suite
  ; "Decreasing values", decrease_suite
  ; "Comparing values", compare_suite ]
