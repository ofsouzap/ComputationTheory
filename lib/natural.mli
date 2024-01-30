type natural =
    | Zero
    | Succ of natural

val nat_succ : natural -> natural

val nat_pred_or_zero : natural -> natural

val nat_plus : natural -> natural -> natural

val nat_monus : natural -> natural -> natural

val nat_of_int : int -> natural

val int_of_nat : natural -> int

val string_of_nat : natural -> string

val nat_gen : natural QCheck.Gen.t

val nat_arbitrary : natural QCheck.arbitrary
