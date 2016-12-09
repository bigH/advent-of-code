import Data.List

input = "cqjxjnds"

sliding :: Int -> [a] -> [[a]]
sliding x as
  | length as <= x = [as]
  | otherwise      = take x as : sliding x (tail as)

letters = ['a' .. 'z']

increment c
 | c >= 'a' && c < 'z' = succ c
 | otherwise           = error "increment should not be doing this."

runs = sliding 3 letters

next pass = let lsd = last pass
                carry = (lsd == 'z')
            in if carry then
                 (next $ init pass) ++ ['a']
               else
                 init pass ++ [increment lsd]

rotate :: Int -> [a] -> [a]
rotate n list = drop n list ++ take n list

passwords = iterate next input

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) f g a = (f a) && (g a)

(.||.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.||.) f g a = (f a) || (g a)

mapped :: (a -> b) -> a -> (a, b)
mapped f a = (a, f a)

isSecure = hasARun .&&. hasTwoRepeats .&&. hasNoBadLetters

hasARun :: String -> Bool
hasARun s = any (`isInfixOf` s) runs

hasTwoRepeats :: String -> Bool
hasTwoRepeats s = (length $ map head $ group $ sort $ filter isRepeat $ sliding 2 s) >= 2

isRepeat :: String -> Bool
isRepeat (a:b:[]) = a == b

hasNoBadLetters :: String -> Bool
hasNoBadLetters = not . (elem 'i' .||. elem 'o' .||. elem 'l')

valid = filter isSecure passwords

main = do putStrLn "Next Next Password:"
          putStrLn $ head $ tail valid


