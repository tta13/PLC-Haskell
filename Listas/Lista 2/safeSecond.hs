import Prelude hiding (Maybe (..))

data Maybe a = Just a |
               Nothing
               deriving(Show)

safeSecond :: [a] -> Maybe a
safeSecond []     = Nothing
safeSecond [a]    = Nothing
safeSecond (a:as) = Just (head as)               

main = do
       a <- getLine
       let result = safeSecond (read a::[Int])
       print result