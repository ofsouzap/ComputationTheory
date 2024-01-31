open Natural

type register_label = int
(** The label for a register *)

type register = natural
(** The value of a single register *)

type memory = register_label -> register
(** A memory mapping register labels to register values *)

val memory_inc : register_label -> memory -> memory
(** Increment a register's value in a memory *)

val memory_dec : register_label -> memory -> memory * bool
(** Take a register's value in memory. If it is zero then return the memory and false in the pair,
    otherwise decrement the value and return true in the pair *)

type instruction_label = int
(** The label for an instruction *)

type instruction =
  | Halt
  | Inc of register_label * instruction_label
  | Dec of register_label * instruction_label * instruction_label
(** A single instruction *)

type program = instruction_label -> instruction
(** A program mapping instruction labels to instructions *)

type configuration = instruction_label * memory
(** An instantaneous configuration of the register machine with the instruction about to be read and the current state of memory *)

val instruction_execute : memory -> instruction -> (memory, configuration) Either.t
(** Execute an instruction on a configuration. If halting, return left of the memory, otherwise return right of the new configuration *)

val configuration_step : program -> configuration -> (memory, configuration) Either.t
(** Have a configuration take a single step using the given program *)
