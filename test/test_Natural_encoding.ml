(* open ComputationTheory.Natural
open ComputationTheory.Natural_encoding

(* Numbers *)

let test_number_ed_id =
  QCheck.Test.make ~count:1000
  QCheck.int
  ( fun x' -> let x = abs x' in int_of_nat (decode_nat (encode_nat (nat_of_int x))) = x )

let test_number_de_id =
  QCheck.Test.make ~count:1000
  QCheck.int
  ( fun x' -> let x = abs x' in int_of_nat (encode_nat (decode_nat (nat_of_int x))) = x )

let number_suite = List.map QCheck_alcotest.to_alcotest
  [ test_number_ed_id
  ; test_number_de_id ]

(* Pairs *)

let test_pair_ed_id =
  QCheck.Test.make ~count:1000
  QCheck.(pair int int)
  ( fun (x',y') -> let x,y = abs x',abs y' in decode_pair (encode_pair (nat_of_int x, nat_of_int y)) = (nat_of_int x, nat_of_int y) )

let test_pair_de_id =
  QCheck.Test.make ~count:1000
  QCheck.int
  ( fun x' -> let x = abs x' in int_of_nat (encode_pair (decode_pair (nat_of_int x))) = x )

let test_pair_non_zero_ed_id =
  QCheck.Test.make ~count:1000
  QCheck.(pair int int)
  ( fun (x',y') -> let x,y = abs x',abs y' in decode_pair_non_zero (encode_pair_non_zero (nat_of_int x, nat_of_int y)) = (nat_of_int x, nat_of_int y) )

let test_pair_non_zero_de_id =
  QCheck.Test.make ~count:1000
  QCheck.int
  ( fun x' -> let x = abs x' in int_of_nat (encode_pair_non_zero (decode_pair_non_zero (nat_of_int x))) = x )

let test_pair_non_zero_not_zero =
  QCheck.Test.make ~count:1000
  QCheck.(pair int int)
  ( fun (x',y') -> let x,y = (abs x',abs y') in encode_pair_non_zero (nat_of_int x,nat_of_int y) != nat_zero )

let pair_suite = List.map QCheck_alcotest.to_alcotest
  [ test_pair_non_zero_ed_id
  ; test_pair_non_zero_de_id
  ; test_pair_ed_id
  ; test_pair_de_id
  ; test_pair_non_zero_not_zero ]

(* Lists *)

let test_list_ed_id =
  QCheck.Test.make ~count:1000
  QCheck.(list int)
  ( fun xs' -> let xs = List.map abs xs' in List.map int_of_nat (decode_list (encode_list (List.map nat_of_int xs))) = xs )

let test_list_de_id =
  QCheck.Test.make ~count:1000
  QCheck.int
  ( fun x' -> let x = abs x' in int_of_nat (encode_list (decode_list (nat_of_int x))) = x )

let list_suite = List.map QCheck_alcotest.to_alcotest
  [ test_list_ed_id
  ; test_list_de_id ] *)

let () =
  let open Alcotest in
  run "Natural Encodings" []
    (* [ "Numbers", number_suite
    ; "Pairs", pair_suite
    ; "Lists", list_suite ] *)
