-- DISCOVERY: the circuit is designed to calculate: 12! + 85 * 92
import Data.Maybe

data Param = Value Int | A | B | C | D deriving (Show, Eq, Ord)

data Instruction = Copy Param Param |
                   Toggle Param     |
                   Increment Param  |
                   Decrement Param  |
                   Jump Param Param deriving (Show, Eq, Ord)

data Registers = Registers Int Int Int Int deriving (Show, Eq)

type State = (Int, [Instruction], Registers)

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
process (Copy      a b) (ix, inst, s) = (ix + 1, inst, store (load a s) b s)
process (Increment a  ) (ix, inst, s) = (ix + 1, inst, store ((load a s) + 1) a s)
process (Decrement a  ) (ix, inst, s) = (ix + 1, inst, store ((load a s) - 1) a s)
process (Jump      a b) (ix, inst, s) = if load a s == 0
                                  then (ix + 1, inst, s)
                                  else (ix + load b s, inst, s)
process (Toggle    a  ) (ix, inst, s) = (ix + 1, toggled (ix + load a s), s)
  where toggled index = if index >= length inst then inst
                                                else take index inst ++ [toggle $ inst !! index] ++ drop (index + 1) inst
        toggle (Copy    a b) = Jump a b
        toggle (Jump    a b) = Copy a b
        toggle (Increment a) = Decrement a
        toggle (Decrement a) = Increment a
        toggle (Toggle    a) = Increment a

program = [Copy A B,
           Decrement B,
           Copy A D,
           Copy (Value 0) A,
           Copy B C,
           Increment A,
           Decrement C,
           Jump C (Value (-2)),
           Decrement D,
           Jump D (Value (-5)),
           Decrement B,
           Copy B C,
           Copy C D,
           Decrement D,
           Increment C,
           Jump D (Value (-2)),
           Toggle C,
           Copy (Value (-16)) C,
           Jump (Value 1) C,
           Copy (Value 85) C,
           Jump (Value 92) D,
           Increment A,
           Increment D,
           Jump D (Value (-2)),
           Increment C,
           Jump C (Value (-5))]

initial = Just (0, program, Registers 12 0 0 0)

run :: Maybe State -> Maybe State
run Nothing                = Nothing
run (Just state @ (ix, inst, s)) = fmap (flip process state) instruction
  where instruction = lookup ix $ [0..] `zip` inst

final = fromJust $ until isNothing run initial
-- final = fromJust $ while isJust run initial

while :: (a -> Bool) -> (a -> a) -> a -> a
while p f a
 | not . p $ fa = a
 | otherwise    = while p f fa
 where fa = f a

-- map fromJust $ takeWhile isJust $ iterate run initial

render :: State -> String
render (index, instructions, registers) =
  "\n\n\n\n" ++ show index ++ "\n\n" ++ show registers ++ "\n\n" ++ (unlines $ map show instructions)

-- DISCOVERY: the circuit is designed to calculate: 12! + 85 * 92
main = do putStrLn "After Execution, Register `A` Contains:"
          let (ix, instructions, registers) = final
          print $ load A registers
