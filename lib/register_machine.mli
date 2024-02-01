open Natural

type register_label = int
(** The label for a register *)

val register_zero : register_label
(** The zero register's label *)

type register = natural
(** The value of a single register *)

val register_default : register
(** Default value for a register *)

type memory = register_label -> register
(** A memory mapping register labels to register values *)

val memory_read_output : memory -> register
(** Read the output register's value from a memory *)

val memory_inc : register_label -> memory -> memory
(** Increment a register's value in a memory *)

val memory_dec : register_label -> memory -> memory * bool
(** Take a register's value in memory. If it is zero then return the memory and false in the pair,
    otherwise decrement the value and return true in the pair *)

type instruction_label = int
(** The label for an instruction *)

val instruction_zero : instruction_label
(** The zero instruction's label *)

type instruction =
  | Halt
  | Inc of register_label * instruction_label
  | Dec of register_label * instruction_label * instruction_label
(** A single instruction *)

val instruction_default : instruction
(** Default value for a instruction *)

type program = instruction_label -> instruction
(** A program mapping instruction labels to instructions *)

type configuration = instruction_label * memory
(** An instantaneous configuration of the register machine with the instruction about to be read and the current state of memory *)

val instruction_execute : memory -> instruction -> (memory, configuration) Either.t
(** Execute an instruction on a configuration. If halting, return left of the memory, otherwise return right of the new configuration *)

val configuration_step : program -> configuration -> (memory, configuration) Either.t
(** Have a configuration take a single step using the given program *)

val configuration_run : program -> configuration -> memory
(** Run a program given an initial configuration until it reaches a HALT. If fed a non-terminating program-memory combination, this won't terminate.
    If only there were an algorithm to check if the arbitrary program would terminate given its input or not so this could be checked before running the program *)

val build_memory : register list -> memory
(** Build a memory of registers from a list of register values. The first value is given label 0 and all other registers are given value 0 *)

val build_program : instruction list -> program
(** Build a program from a list of instructions. The first instruction will be given label 0 and all other instructions will be taken as HALT instructions *)
