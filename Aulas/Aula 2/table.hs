addEsp :: Int -> String
addEsp n 
    | n <= 0    = ""
    | otherwise = ' ':addEsp (n - 1)

shift :: String -> Int -> String
shift text n = addEsp n ++ text

cabecalho = "Semana" ++ shift "Venda" 3 ++ "\n"

vendas :: Int -> Int
vendas n = mod n 17

somaVendas 0 = vendas 0
somaVendas n = vendas n + somaVendas (n - 1)

imprimeVendas :: Int -> String
imprimeVendas 0 = show 0 ++ shift (show (vendas 0)) 8 ++ "\n"
imprimeVendas n = imprimeVendas (n - 1) ++ show n ++ shift (show (vendas n)) 8 ++ "\n"

imprimeTotal :: Int -> String
imprimeTotal n = "Total" ++ shift (show (somaVendas n)) 4 ++ "\n"

imprimeMedia :: Int -> String
imprimeMedia n = "Média" ++ shift (show (fromIntegral(somaVendas n) / fromIntegral(n + 1))) 4 ++ "\n"

imprimeTabela :: Int -> String
imprimeTabela n = cabecalho ++ imprimeVendas n ++ imprimeTotal n ++ imprimeMedia n