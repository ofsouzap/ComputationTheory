module ComputationTheory.RegisterMachines
  (  ) where

data Instruction l =
    Halt
  | Inc l
  | Dec (l, l)

newtype Program l = Program (l -> Instruction l)

newtype Register = Register Integer

newtype Memory r = Memory (r -> Register)

newtype Config l r = Config (l, Memory r)

step :: Config l r -> Instruction l -> Either (Instruction l) (Config l r)
step = undefined -- TODO

run :: Config l r -> Program l -> Either (Instruction l) (Config l r)
run = undefined -- TODO
