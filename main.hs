import Control.Applicative (liftA2)
import Text.Parsec
import Text.Parsec.Char (char, letter)
import Text.Parsec.String (Parser)

------------------------------------------------------------------
-------------------------- Função Main ---------------------------
------------------------------------------------------------------

main :: IO ()
main = do
  -- Lê o arquivo "input.txt"
  input <- readFile "inputs/input1.txt"

  -- Transformar input (string) num datatype (Formula)
  -- utilizando 'case of' para fazer 'pattern matching' no resultado da função 'parseFormula input'
  case parseFormula input of
    Left erro -> print erro
    Right formula -> do
      putStrLn ("Fórmula Original: " ++ show formula)

------------------------------------------------------------------
---------- Definição da estrutura de dados da fórmula ------------
------------------------------------------------------------------

data Formula
  = Var Char -- Variável proposicional representada por um caractere
  | Nao Formula -- Negação de uma fórmula
  | E Formula Formula -- Conjunção (E) de duas fórmulas
  | Ou Formula Formula -- Disjunção (OU) de duas fórmulas
  | Imp Formula Formula -- Implicação de uma fórmula para outra
  deriving (Show, Eq)

-----------------------------------------------------------------
--- Função principal para converter uma string em uma fórmula ---
-----------------------------------------------------------------

-- ORDEM DE PRECEDÊNCIA: ¬, ∧, ∨, →

parseFormula :: String -> Either ParseError Formula
parseFormula = parse parseImplicacao ""

-- Parser para implicação
parseImplicacao :: Parser Formula
parseImplicacao = chainr1 parseOu (Imp <$ (string "→"))

-- Parser para disjunção
parseOu :: Parser Formula
parseOu = chainl1 parseE (Ou <$ (char '∨'))

-- Parser para conjunção
parseE :: Parser Formula
parseE = chainl1 parseNao (E <$ (char '∧'))

-- Parser para negação
parseNao :: Parser Formula
parseNao = (char '¬' *> (Nao <$> parseNao)) <|> parseAtomo

-- Parser para átomos (variável ou expressão entre parênteses)
parseAtomo :: Parser Formula
parseAtomo = parseVariavel <|> parseParentese

-- Parser para variáveis
parseVariavel :: Parser Formula
parseVariavel = Var <$> letter

-- Parser para parênteses
parseParentese :: Parser Formula
parseParentese = between (char '(') (char ')') parseImplicacao

------------------------------------------------------------------
------------------ Definir as regras do Tableau ------------------
------------------------------------------------------------------

-- Usar a função "simplificar" para aplicar uma regra, e se a fórmula resultante
--      for igual à fórmula original, retorna a fórmula.
--      Caso contrário, aplica as regras novamente à fórmula simplificada.

-- Simplificar a fórmula aplicando as regras
simplificar :: Formula -> Formula
simplificar (Nao (Nao a)) = simplificar a -- ~(~A) = A
simplificar (Ou a b) = Ou (simplificar a) (simplificar b) -- A v B
simplificar (E a b) = E (simplificar a) (simplificar b) -- A ^ B
simplificar (Nao (Ou a b)) = E (simplificar (Nao a)) (simplificar (Nao b)) -- ~(A v B) = ~A ^ ~B
simplificar (Nao (E a b)) = Ou (simplificar (Nao a)) (simplificar (Nao b)) -- ~(A ^ B) = ~A v ~B
simplificar (Imp a b) = Ou (simplificar (Nao a)) (simplificar b) -- (A -> B) = ~A v B
simplificar (Nao (Imp a b)) = E (simplificar a) (simplificar (Nao b)) -- ~(A -> B) = A ^ ~B
simplificar f = f -- Caso base: se nenhuma regra se aplica, retorna a fórmula como está

------------------------------------------------------------------
---------- Definir uma estrutura de dados pro Tableau ------------
---------- para representar se o nó é interno ou folha -----------
------------------------------------------------------------------



------------------------------------------------------------------
-----  Construir árvore do Tableau, o nó raiz será a negação -----
---- da fórmula inicial, Deve-se expandir o nó aplicando as   ----
----     regras até que não seja mais possível expandir       ----
------------------------------------------------------------------



------------------------------------------------------------------
-------  Verificar se cada nó do ramo é fechado.          --------
------ Se todos forem fechados a fórmula é válida.         -------
------ Se no mínimo um nó for aberto a fórmula é inválida. -------
------------------------------------------------------------------