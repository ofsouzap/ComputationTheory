open Natural

val encode_nat : natural -> natural
(** Encode a natural number *)

val decode_nat : natural -> natural
(** Decode a natural number *)

val encode_pair_non_zero : natural * natural -> natural
(** Encode a natural number pair to a non-zero natural number *)

val decode_pair_non_zero : natural -> natural * natural
(** Decode a natural number pair from a non-zero natural number *)

val encode_pair : natural * natural -> natural
(** Encode a natural number pair to a natural number *)

val decode_pair : natural -> natural * natural
(** Decode a natural number pair from a natural number *)

val encode_list : natural list -> natural
(** Encode a list of natural numbers *)

val decode_list : natural -> natural list
(** Decode a list of natural numbers *)
