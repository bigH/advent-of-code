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

password = take 8 $ map (!! 5) $ filter (startswith "00000") $ map hashed joined

main = do putStrLn "Password (door 1):"
          putStrLn password
