import Data.List (find)
import Data.Maybe (fromJust)

flipS :: String -> String
flipS s = map flipBit s

flipBit :: Char -> Char
flipBit '0' = '1'
flipBit '1' = '0'

expand :: String -> String
expand a = a ++ "0" ++ b
  where b = reverse $ flipS a

expansions = iterate expand

fill size seed = fromJust $ find ((== size) . length) $ map (take size) $ expansions seed

fold :: (a -> Maybe a) -> a -> [a]
fold f a = a : rest (f a)
  where rest (Just b) = fold f b
        rest Nothing  = []

reduce :: String -> Maybe String
reduce s = if odd $ length s then Nothing
                             else Just $ reduced s

reduced :: String -> String
reduced [] = []
reduced (a:b:t)
  | a == b =    "1" ++ reduced t
  | otherwise = "0" ++ reduced t
reduced unexpected = error $ "unexpected reduction: " ++ unexpected

checksum = last . fold reduce

main = do putStrLn "Checksum of Expanded Seed:"
          print $ checksum $ fill 272 "11011110011011101"
