import Data.List
import Data.Hash.MD5

-- salt = "qzyelonm"
hashed = md5s . Str

rehash = iterate hashed

stretched s = rehash s !! 2016

nums = [0 ..]

plain salt = map (hashed . (salt ++) . show) nums
cypher = map stretched . plain

indexed salt = nums `zip` cypher salt

otpIndices :: [(Int, String)] -> [Int]
otpIndices ((i, s) : t) =
  if isOTP
    then i : otpIndices t
    else     otpIndices t
      where threes = take 1 $ three s
            isOTP = hasThrees && hasFives
            hasThrees = not $ null $ threes
            hasFives = any matchingFives nextThousand
            matchingFives s = not $ null $ threes `intersect` five s
            nextThousand = map snd $ take 1000 t

npeats :: Eq a => Int -> [a] -> [a]
npeats n = map head . filter ((>= n) . length) . group

three :: String -> [Char]
three = npeats 3

five :: String -> [Char]
five = npeats 5

main = do putStrLn "Index of 64th OTP:"
          print $ otpIndices (indexed "qzyelonm") !! 63
