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

let configuration_step_unfold (prog : program) (conf : configuration) : configuration list * memory =
  let rec aux acc c = match configuration_step prog c with
    | Either.Left mem -> acc, mem
    | Either.Right c' -> aux (c'::acc) c'
  in
  let cs,m = aux [] conf in
  List.rev cs, m

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

let register_label_gen = QCheck.Gen.(map Natural.int_of_nat Natural.nat_gen)

let register_gen = Natural.nat_gen

let memory_gen = QCheck.Gen.(map build_memory (list register_gen))

let memory_arbitrary = QCheck.make memory_gen

let instruction_label_gen = QCheck.Gen.(map Natural.int_of_nat Natural.nat_gen)

let instruction_gen = QCheck.Gen.(frequency
  [ 1, return Halt
  ; 2, map (fun (r,l) -> Inc (r,l)) (pair register_label_gen instruction_label_gen)
  ; 2, map (fun (r,l0,l1) -> Dec (r,l0,l1)) (triple register_label_gen instruction_label_gen instruction_label_gen) ])

let program_gen = QCheck.Gen.(map build_program (list instruction_gen))

let program_arbitrary = QCheck.make program_gen

let configuration_gen = QCheck.Gen.(pair instruction_label_gen memory_gen)

let configuration_arbitrary = QCheck.make configuration_gen
