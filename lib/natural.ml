type natural = Natural of Z.t

let min_nat = Natural Z.zero

let nat_succ (Natural n) = Natural (Z.add n Z.one)

let nat_pred_or_zero (Natural n) = if n = Z.zero
  then Natural Z.zero
  else Natural (Z.sub n Z.one)

let nat_plus (Natural a) (Natural b) = Natural (Z.add a b)

let nat_monus (Natural a) (Natural b) = if b > a
  then (Natural Z.zero)
  else (Natural (Z.sub a b))

let nat_times (Natural a) (Natural b) = Natural (Z.mul a b)

let nat_div (Natural a) (Natural b) = Natural (Z.div a b)

let nat_mod (Natural a) (Natural b) = Natural (Z.(mod) a b)

let nat_pow (Natural a) (Natural b) = Natural (Z.pow a (Z.to_int b))

let nat_of_z x = if Z.geq x Z.zero
  then Natural x
  else failwith "Natural number must be non-negative"

let nat_of_int x = nat_of_z (Z.of_int x)

let nat_of_int_abs x = nat_of_int (abs x)

let z_of_nat (Natural x) = x

let int_of_nat n = Z.to_int (z_of_nat n)

let string_of_nat n = string_of_int (int_of_nat n)

let nat_zero = nat_of_int 0

let nat_one = nat_of_int 1

let nat_two = nat_of_int 2

let nat_gen = QCheck.Gen.map nat_of_int_abs QCheck.Gen.nat

let nat_arbitrary = QCheck.make nat_gen ~print:string_of_nat
