import Data.List
import Data.Maybe
import Text.JSON
import GHC.Real

numbers :: JSValue -> [Int]
numbers js =
  case js of
    JSNull{}           -> []
    JSBool b           -> []
    JSRational False r -> [(read :: String -> Int) $ show $ numerator r]
    JSRational True  r -> error "Expected only integers."
    JSArray a          -> concatMap numbers a
    JSString s         -> []
    JSObject o         -> if any isRed $ map snd $ fromJSObject o then []
                          else concatMap numbers $ map snd $ fromJSObject o

isRed :: JSValue -> Bool
isRed (JSString a) = "red" == (fromJSString a)
isRed _            = False

parse s = decode s :: Result (JSValue)

extract :: Result a -> a
extract (Ok r) = r
extract _      = error "JSON didn't parse."

main = do input <- readFile "../input"
          putStrLn "Sum of JSON Numbers in Input:"
          print $ sum $ numbers $ extract $ parse $ input
