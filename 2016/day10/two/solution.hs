import System.IO.Unsafe
import Data.List
import Data.Maybe
import Data.String.Utils

data Chip = Chip Int deriving (Eq, Show, Ord)
data Bot = Bot Int deriving (Eq, Show, Ord)
data Output = Bucket Int | Another Bot deriving (Eq, Show)

data Instruction = Initial Chip Bot | Give Bot Output Output deriving (Eq, Show)
data State = Hold Bot Chip | Output Int Chip deriving (Eq, Show, Ord)

asInt :: String -> Int
asInt s = read s :: Int

parse :: String -> Instruction
parse s = case split " " s of
  ("bot" : b : _ : _ : _ : "bot" : low : _ : _ : _ : "bot" : high : []) ->
    Give (Bot (asInt b)) (Another (Bot (asInt low))) (Another (Bot (asInt high)))
  ("bot" : b : _ : _ : _ : "bot" : low : _ : _ : _ : "output" : high : []) ->
    Give (Bot (asInt b)) (Another (Bot (asInt low))) (Bucket (asInt high))
  ("bot" : b : _ : _ : _ : "output" : low : _ : _ : _ : "bot" : high : []) ->
    Give (Bot (asInt b)) (Bucket (asInt low)) (Another (Bot (asInt high)))
  ("bot" : b : _ : _ : _ : "output" : low : _ : _ : _ : "output" : high : []) ->
    Give (Bot (asInt b)) (Bucket (asInt low)) (Bucket (asInt high))
  ("value" : v : _ : _ : _ : b : []) ->
    Initial (Chip (asInt v)) (Bot (asInt b))

without :: Eq a => a -> [a] -> [a]
without a as = filter (/= a) as

withoutAll :: Eq a => [a] -> [a] -> [a]
withoutAll bs as = filter (not . (`elem` bs)) as

foldForward :: ([State], [Instruction]) -> [([State], [Instruction])]
foldForward (pre, instructions) = scanl apply (pre, instructions) instructions

apply :: ([State], [Instruction]) -> Instruction -> ([State], [Instruction])
apply (pre, instructions) (Initial c b) = error $ "Initial '" ++ show c ++ "' '" ++ show b ++ "'"
apply (pre, instructions) instruction @ (Give bot lowR highR)
  | length (filter (bot `isBotOf`) pre) > 2 = error $ "`" ++ show bot ++ "` has too many chips"
  | length (filter (bot `isBotOf`) pre) < 2 = (pre, instructions)
  | otherwise = if length chips == 2 then (post, remaining)
                                     else (pre, instructions)
                  where chips = filter (bot `isBotOf`) pre
                        (Hold _ lowC : Hold _ highC : []) = sortOn chipOf chips
                        remaining = without instruction instructions
                        removed = withoutAll chips pre
                        post = case (lowR, highR) of
                          (Another low, Another high) -> removed ++ [Hold low lowC, Hold high highC]
                          (Another low, Bucket high)  -> removed ++ [Hold low lowC, Output high highC]
                          (Bucket low, Another high)  -> removed ++ [Output low lowC, Hold high highC]
                          (Bucket low, Bucket high)   -> removed ++ [Output low lowC, Output high highC]

chipOf :: State -> Chip
chipOf (Hold _ c) = c
chipOf x          = error $ "Unexpected `chipOf` `" ++ show x ++ "`"

botOf :: State -> Bot
botOf (Hold b _) = b
botOf x          = error $ "Unexpected `botOf` `" ++ show x ++ "`"

isChipOf :: Chip -> State -> Bool
isChipOf a (Hold _ b) = a == b
isChipOf _ _          = False

isBotOf :: Bot -> State -> Bool
isBotOf a (Hold b _) = a == b
isBotOf _ _          = False

isInitial :: Instruction -> Bool
isInitial (Initial _ _) = True
isInitial _             = False

concaterate :: (a -> [a]) -> a -> [a]
concaterate f a = let fa = f a
                  in fa ++ concaterate f (last fa)

states :: [Instruction] -> [([State], [Instruction])]
states instructions = concaterate foldForward (initial setup, remaining)
  where (setup, remaining) = partition isInitial instructions

initial :: [Instruction] -> [State]
initial = map startings

startings :: Instruction -> State
startings (Initial c b) = Hold b c
startings x             = error $ "Unexpected input: '" ++ show x ++ "'"

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f = concatMap (maybeToList . f)

findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap f = fmap fromJust . find isJust . map f

combine :: (b -> b -> b) -> (a -> b) -> (a -> b) -> a -> b
combine reducer f g a = reducer (f a) (g a)

isOutput :: State -> Bool
isOutput (Output _ _) = True
isOutput _            = False

noHolds :: [State] -> Bool
noHolds state = all isOutput state

outputNum :: State -> Int
outputNum (Output num _) = num
outputNum _              = error $ "Cannot `outputNum` on a `Hold`"

chipNum :: State -> Int
chipNum (Output _ (Chip num)) = num
chipNum _                     = error $ "Cannot `chipNum` on a `Hold`"

main = do input <- readFile "../input"
          putStrLn "Product of Chip Numbers in Outputs 0, 1, 2"
          print $ product $ map chipNum $ filter ((`elem` [0, 1, 2]) . outputNum) $ head $ filter noHolds $ map fst $ states $ map parse $ lines input
