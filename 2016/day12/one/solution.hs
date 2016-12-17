import Data.Maybe

data Param = Value Int | A | B | C | D deriving (Show, Eq, Ord)

data Instruction = Copy Param Param |
                   Increment Param  |
                   Decrement Param  |
                   Jump Param Param deriving (Show, Eq, Ord)

data Registers = Registers Int Int Int Int deriving (Show, Eq)

type State = (Int, Registers)

load :: Param -> Registers -> Int
load (Value i) _                 = i
load A         (Registers a _ _ _) = a
load B         (Registers _ b _ _) = b
load C         (Registers _ _ c _) = c
load D         (Registers _ _ _ d) = d

store :: Int -> Param -> Registers -> Registers
store i A (Registers a b c d) = Registers i b c d
store i B (Registers a b c d) = Registers a i c d
store i C (Registers a b c d) = Registers a b i d
store i D (Registers a b c d) = Registers a b c i
store i r _               = error $ "Cannot store " ++ show i ++ " into " ++ show r

process :: Instruction -> State -> State
process (Copy      a b) (ix, s) = (ix + 1, store (load a s) b s)
process (Increment a  ) (ix, s) = (ix + 1, store ((load a s) + 1) a s)
process (Decrement a  ) (ix, s) = (ix + 1, store ((load a s) - 1) a s)
process (Jump      a b) (ix, s) = if load a s == 0
                                  then (ix + 1, s)
                                  else (ix + load b s, s)

program = [Copy (Value 1) A,
           Copy (Value 1) B,
           Copy (Value 26) D,
           Jump C (Value 2),
           Jump (Value 1) (Value 5),
           Copy (Value 7) C,
           Increment D,
           Decrement C,
           Jump C (Value (-2)),
           Copy A C,
           Increment A,
           Decrement B,
           Jump B (Value (-2)),
           Copy C B,
           Decrement D,
           Jump D (Value (-6)),
           Copy (Value 19) C,
           Copy (Value 11) D,
           Increment A,
           Decrement D,
           Jump D (Value (-2)),
           Decrement C,
           Jump C (Value (-5))]

instructions = [0..] `zip` program

initial = Just (0, Registers 0 0 0 0)

run :: Maybe State -> Maybe State
run Nothing                = Nothing
run (Just state @ (ix, _)) = fmap (flip process state) instruction
  where instruction = lookup ix instructions

states = map fromJust $ takeWhile isJust $ iterate run initial

main = do putStrLn "After Execution, Register `A` Contains:"
          let (ix, registers) = last states
          print $ load A registers
