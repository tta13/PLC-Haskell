type Pessoa = String
type Livro = String
type Banco = [(Pessoa, String)]

baseExemplo :: Banco
baseExemplo = [("Neymar", "1984"), ("Lebron", "1984"), ("Harden", "Revolucao dos Bichos"), ("Mandynha Moraes", "Metamorfose"), ("Lebron", "Metamorfose"), ("Harden", "Diario de um Banana")]

livros :: Banco -> Pessoa -> [Livro]
livros b p = [l | (any,l) <- b, any == p]

emprestimos :: Banco -> Livro -> [Pessoa]
emprestimos b l = [p | (p,b) <- b, b == l]

emprestado :: Banco -> Livro -> Bool
emprestado [] _         = False
emprestado b l
    | l == snd (head b) = True
    | otherwise         = emprestado (tail b) l

qtdEmprestimos :: Banco -> Pessoa -> Int
qtdEmprestimos b p = length (livros b p)

emprestar :: Banco -> Pessoa -> Livro -> Banco
emprestar b p l = b ++ [(p, l)]

devolver :: Banco -> Pessoa -> Livro -> Banco
devolver b p l = [(anyP, anyB) | (anyP, anyB) <- b, (anyP /= p || anyB /= l)]