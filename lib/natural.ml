type natural = Natural of int

let min_nat = Natural 0

let max_nat = Natural max_int

let nat_succ (Natural n) = Natural (n + 1)

let nat_pred_or_zero (Natural n) = if n = 0
  then Natural 0
  else Natural (n - 1)

let nat_plus (Natural a) (Natural b) = Natural (a + b)

let nat_monus (Natural a) (Natural b) = if b > a
  then (Natural 0)
  else (Natural (a - b))

let nat_of_int x = if x >= 0
  then Natural x
  else failwith "Natural number must be non-negative"

let nat_of_int_abs x = nat_of_int (abs x)

let int_of_nat (Natural n) = n

let string_of_nat n = string_of_int (int_of_nat n)

let nat_gen = QCheck.Gen.map nat_of_int_abs QCheck.Gen.nat

let nat_arbitrary = QCheck.make nat_gen ~print:string_of_nat
