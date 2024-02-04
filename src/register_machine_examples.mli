open Register_machine

val first_projection : program
(** The input values, x and y, should be stored in R1 and R2 respectively. The output is put into R0 *)

val const : int -> program
(** Input value, x, is stored in R1. Output is put in R0 *)

val truncated_subtraction : program
(** The input values, x and y, should be stored in R1 and R2 respectively. The output is put into R0 *)

val integer_division : program
(** The input values, x and y, should be stored in R1 and R2 respectively. The output is put into R0 *)

val modulo : program
(** The input values, x and y, should be stored in R1 and R2 respectively. The output is put into R0 *)

val exp2 : program
(** The input value, x, should be stored in R1. The output, 2^x, is put into R0 *)
