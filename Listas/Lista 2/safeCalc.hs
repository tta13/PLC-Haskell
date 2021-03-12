import Prelude hiding (Maybe (..))

data Operation = Operation Int String Int | Nope

data Maybe a = Just a |
               Nothing
               deriving(Show)

getFirst :: String -> Int
getFirst str = read (takeWhile (\x -> x >= '0' && x <= '9') str)::Int

getOp :: String -> String
getOp str = take 3 (dropWhile (\x -> x >= '0' && x <= '9') str)

getSecond :: String -> Int
getSecond str = read (drop 3 (dropWhile (\x -> x >= '0' && x <= '9') str))::Int

calculate :: Int -> Int -> String -> Maybe Int
calculate a b "sum" = Just (a+b)
calculate a b "sub" = Just (a-b)
calculate a b "mul" = Just (a*b)
calculate a b "div" = if b /= 0 then Just (div a b) else Nothing
calculate a b _     = Nothing

safeCalc :: String -> IO ()
safeCalc str = putStr (show (calculate (getFirst str) (getSecond str) (getOp str)))

main = do
       a <- getLine
       safeCalc a