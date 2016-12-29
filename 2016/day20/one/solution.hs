import Data.String.Utils

type Range = (Word, Word)
type Space = [(Word, Word)]

start :: Space
start = [(0, 4294967295)]

asWord :: String -> Word
asWord = read

parse :: String -> (Word, Word)
parse s = case split "-" s of
            ([]) -> error $ "s = " ++ s
            (a:[]) -> error $ "s = " ++ s
            (a:b:[]) -> (asWord a, asWord b)
            (a:b:c) -> error $ "s = " ++ s

remove :: Space -> Range -> Space
remove [] _ = []
remove ((a, b) : t) (c, d)
  | d < a                = (a, b) : t                   -- range below current
  | b < c                = (a, b) : remove t (c, d)     -- range above current
  | (c <= a) && (d >= b) = remove t (c, d)              -- range encompassing current
  | (c <= a) && (d <  b) = (d + 1, b) : t               -- range taking from beginning of current
  | (c >  a) && (d >= b) = (a, c - 1) : remove t (c, d) -- range taking from end of current
  | otherwise            = (a, c - 1) : (d + 1, b) : t  -- range splitting current

main = do input <- readFile "../input"
          putStrLn "Lowest Available IP:"
          print $ fst $ head $ foldl remove start $ map parse $ lines input
