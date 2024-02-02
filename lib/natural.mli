type natural
(** A natural number (ie. 0, 1, ...) *)

val min_nat : natural
(** The minimum natural number, zero *)

val max_nat : natural
(** The maximum representable natural number *)

val nat_succ : natural -> natural
(** The successor of a natural number *)

val nat_pred_or_zero : natural -> natural
(** The predecessor of a natural number or zero if the number is zero *)

val nat_plus : natural -> natural -> natural
(** Add two natural numbers *)

val nat_monus : natural -> natural -> natural
(** Subtract two natural numbers or return zero if the result of the integer subtraction would be negative *)

val nat_times : natural -> natural -> natural
(** Multiply two natural numbers *)

val nat_div : natural -> natural -> natural
(** Integer division on natural numbers *)

val nat_mod : natural -> natural -> natural
(** Modulo operator on natural numbers *)

val nat_pow : natural -> natural -> natural
(** Compute the integer power of two natural numbers *)

val nat_of_int : int -> natural

val int_of_nat : natural -> int

val string_of_nat : natural -> string

val nat_zero : natural
(** The natural number zero *)

val nat_one : natural
(** The natural number one *)

val nat_two : natural
(** The natural number two *)

val nat_gen : natural QCheck.Gen.t

val nat_arbitrary : natural QCheck.arbitrary

(* TODO - helpers for creating programs *)
