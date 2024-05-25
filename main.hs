import Data.Char (isAlpha)

-- 1) Receber uma fórmula como input
main :: IO ()
main = do
    putStrLn "Digite uma fórmula da Lógica Clássica Proposicional:"
    input <- getLine -- Recebe o input
    let formula = converterInputEmToken input -- Define variável formula pra armazenar o conjunto de tokens
    print formula


-- 2) Definindo uma estrutura pra cada token da fórmula
data Token = Variavel Char           -- Variáveis proposicionais, representadas por caracteres
           | E                  -- Operador lógico "E" ^
           | Ou                 -- Operador lógico "OU" v
           | Nao                -- Operador lógico "NÃO" ~
           | Implicacao         -- Operador lógico "IMPLICA" ->
           | BiImplicacao       -- Operador lógico "SE E SOMENTE SE" <->
           | AbrirParenteses    -- Parêntese de abertura '('
           | FecharParenteses   -- Parêntese de fechamento ')'
           deriving (Show, Eq)


-- 3) Função que converte o input (que é uma string) em tokens
converterInputEmToken :: String -> [Token]
converterInputEmToken [] = []
converterInputEmToken (' ':cs) = converterInputEmToken cs  -- Ignorar espaços em branco
converterInputEmToken ('^':cs) = E : converterInputEmToken cs
converterInputEmToken ('v':cs) = Ou : converterInputEmToken cs
converterInputEmToken ('!':cs) = Nao : converterInputEmToken cs
converterInputEmToken ('-':'>':cs) = Implicacao : converterInputEmToken cs
converterInputEmToken ('<':'-':'>':cs) = BiImplicacao : converterInputEmToken cs
converterInputEmToken ('(':cs) = AbrirParenteses : converterInputEmToken cs
converterInputEmToken (')':cs) = FecharParenteses : converterInputEmToken cs
converterInputEmToken (c:cs)
    | Data.Char.isAlpha c = Variavel c : converterInputEmToken cs
    | otherwise = error $ "Caractere inválido: " ++ [c]


{-


4. Transformar essa sequência de tokens numa estrutura de dados fórmula que representa a fórmula proposicional de maneira hierárquica
5. Definir as regras do Tableau
6. Definir uma estrutura de dados pro Tableau, para representar se o nó é interno ou folha
7. Construir árvore do Tableau, o nó raiz será a negação da fórmula inicial, Deve-se expandir o nó aplicando as regras até que não seja mais possível expandir
8. Verificar se cada nó do ramo é fechado. Se todos forem fechados a fórmula é válida. Se no mínimo um nó for aberto a fórmula é inválida.
9. Imprimir o resultado

-}