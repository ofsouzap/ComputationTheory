type natural =
  | Zero
  | Succ of natural

let nat_succ n = Succ n

let nat_pred_or_zero = function
  | Zero -> Zero
  | Succ n -> n

let rec nat_plus a = function
  | Zero -> a
  | Succ b' -> nat_plus (Succ a) b'

let rec nat_monus a b = match (a, b) with
  | (a, Zero) -> a
  | (Zero, _) -> Zero
  | (Succ a', Succ b') -> nat_monus a' b'

let nat_of_int =
  let rec aux acc = function
    | 0 -> acc
    | n -> if n > 0
      then aux (Succ acc) (n-1)
      else failwith "Natural number must be non-negative"
  in
  aux Zero

let nat_of_int_abs x = nat_of_int (abs x)

let int_of_nat =
  let rec aux acc = function
    | Zero -> acc
    | Succ x' -> aux (acc+1) x'
  in
  aux 0

let string_of_nat n = string_of_int (int_of_nat n)

let nat_gen = QCheck.Gen.map nat_of_int_abs QCheck.Gen.nat

let nat_arbitrary = QCheck.make nat_gen ~print:string_of_nat
