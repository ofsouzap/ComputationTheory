open Natural

let id a = a

let encode_nat = id

let decode_nat = id

let encode_pair_non_zero (a, b) =
  nat_times
    ( nat_pow nat_two a )
    ( nat_plus
      ( nat_times nat_two b )
      nat_one )

let decode_pair_non_zero n =
  let rec extract_x (acc, rem) = if nat_mod rem nat_two = nat_zero
    then extract_x (nat_plus acc nat_one, nat_div rem nat_two)
    else (acc, nat_div rem nat_two)
  in
  extract_x (nat_zero, n)

let encode_pair (a, b) =
  nat_monus ( encode_pair_non_zero (a, b) ) nat_one

let decode_pair n =
  (decode_pair_non_zero (nat_plus n nat_one))

let rec encode_list = function
  | [] -> nat_zero
  | h::ts -> encode_pair_non_zero (h, (encode_list ts))

let decode_list n =
  let rec aux (acc_x, acc_xs) n = if n = nat_zero
    then acc_xs
    else if nat_mod n nat_two = nat_zero
      then aux (nat_plus acc_x nat_one, acc_xs) (nat_div n nat_two)
      else aux (nat_zero, acc_x::acc_xs) (nat_div n nat_two)
  in
  List.rev (aux (nat_zero, []) n)
