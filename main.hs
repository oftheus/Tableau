import Data.Char (isAlpha)


------------------------------------------------------------------
-------------- 1) Recebendo uma fórmula como input ---------------
------------------------------------------------------------------

main :: IO ()
main = do
    -- Lê o arquivo "input.txt"
    input <- readFile "input.txt"

    -- Converte o input p/ uma lista de tokens usando a função 'converterInputToken' 
    let tokens = converterInputToken input  
        formula = converterParaAST tokens -- Transforma a lista de tokens na AST usando a função 'converterParaAST'
    putStrLn "Fórmula em Tokens:"
    print formula

    -- Simplifica a fórmula aplicando as regras do Tableau
    let simplificada = aplicarRegras formula

    putStrLn "Fórmula simplificada usando as regras do Tableau:"
    print simplificada



------------------------------------------------------------------
------ 2) Definindo uma estrutura pra cada token da fórmula ------
------------------------------------------------------------------

data Token = Variavel Char             -- Variáveis proposicionais, representadas por caracteres
           | E_Token                   -- Operador lógico "E" ^
           | Ou_Token                  -- Operador lógico "OU" v
           | Nao_Token                 -- Operador lógico "NÃO" ~
           | Implicacao_Token          -- Operador lógico "IMPLICA" ->
           | BiImplicacao_Token        -- Operador lógico "SE E SOMENTE SE" <->
           | AbrirParenteses_Token     -- Parêntese de abertura '('
           | FecharParenteses_Token    -- Parêntese de fechamento ')'
           deriving (Show, Eq)


------------------------------------------------------------------
--  3) Função que converte o input (que é uma string) em tokens --
------------------------------------------------------------------

converterInputToken :: String -> [Token]

-- Caso base: Se a string está vazia, retorna uma lista vazia de tokens
converterInputToken [] = []

-- Ignora espaços em branco no input
converterInputToken (' ':cs) = converterInputToken cs 

-- Converte o caractere '^' no "E_Token"
converterInputToken ('^':cs) = E_Token : converterInputToken cs

-- Converte o caractere 'v' no "Ou_Token"
converterInputToken ('v':cs) = Ou_Token  : converterInputToken cs

-- Converte o caractere '~' no "Nao_Token"
converterInputToken ('~':cs) = Nao_Token  : converterInputToken cs

-- Converte '->' no "Implicacao_Token"
converterInputToken ('-':'>':cs) = Implicacao_Token  : converterInputToken cs

-- Converte '<->' no "BiImplicacao_Token"
converterInputToken ('<':'-':'>':cs) = BiImplicacao_Token  : converterInputToken cs

-- Converte '(' no AbrirParenteses_Token
converterInputToken ('(':cs) = AbrirParenteses_Token  : converterInputToken cs

-- Converte o caractere ')' no FecharParenteses_Token
converterInputToken (')':cs) = FecharParenteses_Token  : converterInputToken cs

-- Converter uma letra em uma variável proposicional
converterInputToken (c:cs)
    -- Se é uma letra, converte em token de variável e continua tokenizando o resto da string
    | isAlpha c = Variavel c : converterInputToken cs

    -- Se o caractere não é reconhecido, lança um erro
    | otherwise = error $ "Caractere inválido: " ++ [c]


------------------------------------------------------------------
--- 4) Transformar essa sequência de tokens numa estrutura de ----
--- dados 'fórmula' que representa a fórmula proposicional de ----
---                 maneira hierárquica                       ----
------------------------------------------------------------------

-- Definição da estrutura de dados para a árvore de sintaxe abstrata (AST)
data Formula = Var Char              -- Variável proposicional representada por um caractere
             | Nao Formula           -- Negação de uma fórmula
             | E Formula Formula     -- Conjunção (E) de duas fórmulas
             | Ou Formula Formula    -- Disjunção (OU) de duas fórmulas
             | Imp Formula Formula   -- Implicação de uma fórmula para outra
             | BiImp Formula Formula -- Bi-implicação entre duas fórmulas
             deriving (Show, Eq)

-- PARSING
-- ORDEM DE PRECEDÊNCIA: ~, ^, v, ->, <->

-- Função para transformar a sequência de tokens na AST
converterParaAST :: [Token] -> Formula
converterParaAST tokens = 
    case converterExpressoes tokens of
        (formula, []) -> formula   
        _ -> error "Tokens sobrando!"

-- Função para analisar expressões considerando precedência e associatividade
converterExpressoes :: [Token] -> (Formula, [Token])
converterExpressoes tokens = parseBiImplicacao tokens


parseBiImplicacao :: [Token] -> (Formula, [Token])
parseBiImplicacao tokens = 
    let (esquerda, rest) = parseImplicacao tokens
    in case rest of
        (BiImplicacao_Token :rest') -> let (direita, rest'') = parseBiImplicacao rest'
                                in (BiImp esquerda direita, rest'')
        _ -> (esquerda, rest)

parseImplicacao :: [Token] -> (Formula, [Token])
parseImplicacao tokens = 
    let (esquerda, rest) = parseOu tokens
    in case rest of
        (Implicacao_Token :rest') -> let (direita, rest'') = parseImplicacao rest'
                              in (Imp esquerda direita, rest'')
        _ -> (esquerda, rest)


parseOu :: [Token] -> (Formula, [Token])
parseOu tokens = 
    let (esquerda, rest) = parseE tokens
    in case rest of
        (Ou_Token :rest') -> let (direita, rest'') = parseOu rest'
                      in (Ou esquerda direita, rest'')
        _ -> (esquerda, rest)

parseE :: [Token] -> (Formula, [Token])
parseE tokens = 
    let (esquerda, rest) = parseNao tokens
    in case rest of
        (E_Token:rest') -> let (direita, rest'') = parseE rest'
                     in (E esquerda direita, rest'')
        _ -> (esquerda, rest)

parseNao :: [Token] -> (Formula, [Token])
parseNao (Nao_Token :rest) = let (subFormula, rest') = parseAtomo rest
                      in (Nao subFormula, rest')
parseNao tokens = parseAtomo tokens

parseAtomo :: [Token] -> (Formula, [Token])
parseAtomo (Variavel x : rest) = (Var x, rest)
parseAtomo (AbrirParenteses_Token  : rest) = 
    let (subFormula, rest') = converterExpressoes rest
    in case rest' of
        (FecharParenteses_Token  : rest'') -> (subFormula, rest'')
        _ -> error "Parêntese de fechamento esperado!"
parseAtomo _ = error "Token inesperado!"


------------------------------------------------------------------
---------------- 5) Definir as regras do Tableau -----------------
------------------------------------------------------------------

-- Aplicando as regras de decomposição para uma fórmula no Tableau

-- Função "aplicarRegras" tenta simplificar uma fórmula aplicando repetidamente 
-- as regras de decomposição até que a fórmula não mude mais. 

-- Usa a função "simplificar" para aplicar uma regra, e se a fórmula resultante
--      for igual à fórmula original, retorna a fórmula. 
--      Caso contrário, aplicaas regras novamente à fórmula simplificada.

aplicarRegras :: Formula -> Formula
aplicarRegras formula =
    let simplificado = simplificar formula
    in if formula == simplificado
           then formula
           else aplicarRegras simplificado

-- Simplificando a fórmula aplicando as regras
simplificar :: Formula -> Formula
simplificar (Nao (Nao a)) = simplificar a  -- ~(~A) = B
simplificar (Ou a b) = Ou (simplificar a) (simplificar b) -- A v 
simplificar (E a b) = E (simplificar a) (simplificar b) -- A ^ B
simplificar (Nao (Ou a b)) = E (simplificar (Nao a)) (simplificar (Nao b)) -- ~(A v B) = ~A ^ ~B
simplificar (Nao (E a b)) = Ou (simplificar (Nao a)) (simplificar (Nao b)) -- ~(A ^ B) = ~A v ~B
simplificar (Imp a b) = Ou (simplificar (Nao a)) (simplificar b) -- (A -> B) = ~A v B
simplificar (BiImp a b) = Ou (E (simplificar a) (simplificar b)) (E (simplificar (Nao a)) (simplificar (Nao b))) -- (A <-> B) = (A ^ B) v (~A ^ ~B)
simplificar (Nao (Imp a b)) = E (simplificar a) (simplificar (Nao b)) -- ~(A -> B) = A ^ ~B
simplificar (Nao (BiImp a b)) = Ou (E (simplificar a) (simplificar (Nao b))) (E (simplificar (Nao a)) (simplificar b)) -- ~(A <-> B) = (A ^ ~B) v (~A ^ B)
simplificar f = f -- Caso base: se nenhuma regra se aplica, retorna a fórmula como está



------------------------------------------------------------------
-------- 6) Definir uma estrutura de dados pro Tableau -----------
--------  para representar se o nó é interno ou folha  -----------
------------------------------------------------------------------



------------------------------------------------------------------
---- 7) Construir árvore do Tableau, o nó raiz será a negação ----
---- da fórmula inicial, Deve-se expandir o nó aplicando as   ----
----     regras até que não seja mais possível expandir       ----
------------------------------------------------------------------



------------------------------------------------------------------
------ 8) Verificar se cada nó do ramo é fechado.          -------
------ Se todos forem fechados a fórmula é válida.         -------
------ Se no mínimo um nó for aberto a fórmula é inválida. -------
------------------------------------------------------------------
