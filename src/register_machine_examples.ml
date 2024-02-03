open Register_machine

let first_projection = build_program
  ( let halt = 2 in
  [ Dec (1, 1, halt)
  ; Inc (0, 0)
  ; Halt ] )

let const n = build_program (List.init n ( fun i -> Inc (0, i+1) ))

let truncated_subtraction = build_program
  ( let halt = 4 in
  [ Dec (1, 1, 2)
  ; Inc (0, 0)
  ; Dec (2, 3, halt)
  ; Dec (0, 2, halt)
  ; Halt ] )

let integer_division = build_program
  ( let halt = 9 in
  [ Dec (2, 2, halt)
  ; Dec (2, 2, 4)
  ; Inc (3, 3)
  ; Inc (4, 1)
  ; Dec (4, 5, 6)
  ; Inc (2, 4)
  ; Dec (3, 7, 8)
  ; Dec (1, 6, halt)
  ; Inc (0, 1)
  ; Halt ] )

let modulo = build_program
  ( let halt = 12 in
  [ Dec (2, 2, 10)
  ; Dec (2, 2, 4)
  ; Inc (3, 3)
  ; Inc (4, 1)
  ; Dec (4, 5, 6)
  ; Inc (2, 4)
  ; Dec (3, 7, 9)
  ; Dec (1, 8, halt)
  ; Inc (0, 6)
  ; Dec (0, 9, 1)
  ; Dec (1, 11, halt)
  ; Inc (0, 10)
  ; Halt ] )
