import Data.List (find)
import Data.Char
import Data.Maybe (isJust, fromJust)
import Data.Hash.MD5
import Data.ByteString.Char8 (pack, unpack)
import Data.String.Utils (startswith)

input :: String
input = "abbhdwsy"

indices :: [Int]
indices = [0 ..]

joined :: [String]
joined = map join indices

join :: Int -> String
join i = input ++ show i

hashed :: String -> String
hashed = md5s . Str

hexAsInt :: Char -> Int
hexAsInt c
  | c >= '0' && c <= '9' = read [c] :: Int
  | otherwise            = 10 + (ord c - ord 'a')

makeTuple :: String -> (Int, Char)
makeTuple str = (hexAsInt pos, char)
  where pos = str !! 5
        char = str !! 6

mix :: [Maybe Char] -> (Int, Char) -> [Maybe Char]
mix xs (i, x)
  | i >= 8           = xs
  | isJust (xs !! i) = xs
  | otherwise        = (take i xs) ++ [Just x] ++ (drop (i + 1) xs)

start :: [Maybe Char]
start = [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]

characterChanges :: [(Int, Char)]
characterChanges = map makeTuple $ filter (startswith "00000") $ map hashed joined

passwords :: [[Maybe Char]]
passwords = scanl mix start characterChanges

passwordArray :: [Maybe Char]
passwordArray = fromJust $ find (all isJust) passwords

password :: String
password = map fromJust passwordArray

main = do putStrLn "Password (door 2):"
          putStrLn password
