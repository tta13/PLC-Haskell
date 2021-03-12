data Cmd = Cursor Int
           | Backspace Int
           | Delete Int
           | Insert String
           deriving (Read)

clamp :: Int -> Int -> Int -> Int
clamp min max value
    | value < min = min
    | value > max = max
    | otherwise = value

setCursor :: Int -> Int -> String -> Int
setCursor pos offset str = clamp 0 (length str) (pos + offset)

delete :: Int -> Int -> String -> String
delete x cursor str = take (cursor) str ++ drop (cursor+x) str

insert :: String -> Int -> String -> String
insert value cursor str = take cursor str ++ value ++ drop cursor str

backspace :: Int -> Int -> String -> String
backspace x cursor str = reverse (delete x (length str - cursor) (reverse str))

process :: String -> Int-> [Cmd] -> String
process s cursor [] = s
process s cursor (c:cs) = 
    case c of (Cursor x)    -> process s (setCursor cursor x s) cs
              (Backspace x) -> process (backspace x cursor s) (setCursor cursor (-x) s) cs
              (Delete x)    -> process (delete x cursor s) cursor cs
              (Insert value)  -> process (insert value cursor s) cursor cs

editText :: String -> [Cmd] -> String
editText text cmd = process text 0 cmd


main = do
       a <- getLine
       b <- getLine
       let result = editText a (read b)
       print result