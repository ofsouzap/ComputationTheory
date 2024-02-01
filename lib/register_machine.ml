open Natural

type register_label = int

let register_zero = 0

type register = natural

let register_default = nat_zero

type memory = register_label -> register

let memory_read_output mem = mem register_zero

let memory_inc (reg : register_label) (mem : memory) : memory = fun x ->
  let p = if x = reg then nat_one else nat_zero in
  nat_plus (mem x) p

let memory_dec (reg : register_label) (mem : memory) : memory * bool =
  if mem reg = nat_zero
  then (mem, false)
  else
    ( ( fun x -> if x = reg then nat_pred_or_zero (mem x) else mem x )
    , true )

type instruction_label = int

let instruction_zero = 0

type instruction =
  | Halt
  | Inc of register_label * instruction_label
  | Dec of register_label * instruction_label * instruction_label

let instruction_default = Halt

type program = instruction_label -> instruction

type configuration = instruction_label * memory

let instruction_execute (mem : memory) = function
  | Halt -> Either.Left mem
  | Inc (r, l) -> Either.Right (l, memory_inc r mem)
  | Dec (r, l1, l2) -> ( match memory_dec r mem with
    | (mem', true) -> Either.Right (l1, mem')
    | (mem', false) -> Either.Right (l2, mem') )

let configuration_step (prog : program) ((pc, mem) : configuration) = instruction_execute mem (prog pc)

let rec configuration_run (prog : program) (conf : configuration) : memory = match configuration_step prog conf with
  | Either.Left mem_out -> mem_out
  | Either.Right conf' -> configuration_run prog conf'

let build_memory (xs : register list) : memory =
  let rec aux = function
    | (_, []) -> register_default
    | (0, h::_) -> h
    | (i, _::ts) -> aux (i-1, ts)
  in
  fun x -> aux (x, xs)

let build_program (xs : instruction list) : program =
  let rec aux = function
    | (_, []) -> instruction_default
    | (0, h::_) -> h
    | (i, _::ts) -> aux (i-1, ts)
  in
  fun x -> aux (x, xs)
