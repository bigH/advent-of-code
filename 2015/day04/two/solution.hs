import Data.Hash.MD5
import Data.String.Utils (startswith)
import Data.List (find)

input = "bgvyzdsv"

nums = [0 ..]

hashed :: String -> String
hashed = md5s . Str

hashMatches :: Int -> Bool
hashMatches i = startswith "000000" $ hashed (input ++ show i)

main = do putStrLn "Least MD5"
          print $ find hashMatches nums
