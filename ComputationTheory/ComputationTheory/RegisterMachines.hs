module ComputationTheory.RegisterMachines
  (  ) where

-- Typeclasses

class Enum l => InstructionLabel l

class (Eq a, Bounded a) => MemLoc a

-- Data

data Instruction l r =
    Halt
  | Inc (r, l)
  | Dec (r, l, l)

newtype InstructionLabel l => Program l r = Program (l -> Instruction l r)

type Register = Integer

regBaseValue :: Register -> Bool
regBaseValue = (==) 0

newtype MemLoc r => Memory r = Memory (r -> Register)

memGet :: MemLoc r => r -> Memory r -> Register
memGet x (Memory f) = f x

memInc :: MemLoc r => r -> Memory r -> Memory r
memInc r old = Memory (\ x ->
  if x == r
  then memGet x old + 1
  else memGet x old )

memDec :: MemLoc r => r -> Memory r -> (Memory r, Bool)
memDec r old = undefined -- TODO

newtype (InstructionLabel l, MemLoc r) => Config l r = Config (l, Memory r)

step :: (InstructionLabel l, MemLoc r) => Config l r -> Instruction l r -> Either (Memory r) (Config l r)
step (Config (pc, mem)) Halt = Left mem
step (Config (pc, mem)) (Inc (r,l)) = Right (Config
  ( succ pc
  , memInc r mem ))

run :: (InstructionLabel l, MemLoc r) => Config l r -> Program l r -> Either (Memory r) (Config l r)
run = undefined -- TODO
