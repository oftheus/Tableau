import Control.Applicative (liftA2)
import Data.Traversable (for)
import Text.Parsec
    ( char,
      letter,
      between,
      chainl1,
      chainr1,
      (<|>),
      parse,
      ParseError )
import Text.Parsec.Char (char, letter)
import Text.Parsec.String (Parser)

------------------------------------------------------------------
-------------------------- Função Main ---------------------------
------------------------------------------------------------------
main :: IO () --roda input1 como exemplo
main = do verify 1

verify :: Int -> IO ()
verify file = do
  -- Lê o arquivo "input.txt"
  input <- readFile ("inputs/input" ++ show file ++ ".txt")

  -- Transformar input (string) num datatype (Formula)
  -- utilizando 'case of' para fazer 'pattern matching' no resultado da função 'parseFormula input'
  case parseFormula input of
    Left erro -> print erro
    Right formula -> do
      let tableau = expandNode [] [] (F formula)
      putStrLn $ "Formula: " ++ show formula ++ "\n"
      putStrLn $ "Arvore:" ++ "\n"
      putStrLn $ show tableau
      putStrLn $ "Tautologia: " ++ show (detectValidade [tableau])

verifyFormula :: String -> IO ()
verifyFormula inputFormula = do

  -- Transformar input (string) num datatype (Formula)
  -- utilizando 'case of' para fazer 'pattern matching' no resultado da função 'parseFormula input'
  case parseFormula inputFormula of
    Left erro -> print erro
    Right formula -> do
      let tableau = expandNode [] [] (F formula)
      putStrLn $ "Formula: " ++ show formula ++ "\n"
      putStrLn $ "Arvore:" ++ "\n"
      putStrLn $ show tableau
      putStrLn $ "Tautologia: " ++ show (detectValidade [tableau])

------------------------------------------------------------------
---------- Definição da estrutura de dados da fórmula ------------
------------------------------------------------------------------

data Formula
  = Var Char -- Variável proposicional representada por um caractere
  | Nao Formula -- Negação de uma fórmula
  | E Formula Formula -- Conjunção (E) de duas fórmulas
  | Ou Formula Formula -- Disjunção (OU) de duas fórmulas
  | Imp Formula Formula -- Implicação de uma fórmula para outra
  deriving (Eq)

-- Mostra formula como string mais facilmente legível, para facilitar depuração
instance Show Formula where
  show :: Formula -> String
  show (Var c) = [c]
  show (Nao f) = "~" ++ show f
  show (E f1 f2) = "(" ++ show f1 ++ " ^ " ++ show f2 ++ ")"
  show (Ou f1 f2) = "(" ++ show f1 ++ " v " ++ show f2 ++ ")"
  show (Imp f1 f2) = "(" ++ show f1 ++ " -> " ++ show f2 ++ ")"

-- Formula, atualizada para poder ser V ou F
data FormulaVF
  = V Formula 
  | F Formula
  deriving (Eq)

instance Show FormulaVF where
  show :: FormulaVF -> String
  show (V f) = "V: " ++ show f 
  show (F f) = "F: " ++ show f

-----------------------------------------------------------------
--- Função principal para converter uma string em uma fórmula ---
-----------------------------------------------------------------

-- ORDEM DE PRECEDÊNCIA: ¬, ∧, ∨, →

parseFormula :: String -> Either ParseError Formula
parseFormula input = parse parseImplicacao "" input

-- Parser para implicação
parseImplicacao :: Parser Formula
parseImplicacao = chainr1 parseOu (Imp <$ char ':')

-- Parser para disjunção
parseOu :: Parser Formula
parseOu = chainl1 parseE (Ou <$ char 'v')

-- Parser para conjunção
parseE :: Parser Formula
parseE = chainl1 parseNao (E <$ char '^')

-- Parser para negação
parseNao :: Parser Formula
parseNao = (char '~' *> (Nao <$> parseNao)) <|> parseAtomo

-- Parser para átomos (variável ou expressão entre parênteses)
parseAtomo :: Parser Formula
parseAtomo = parseVariavel <|> parseParentese

-- Parser para variáveis
parseVariavel :: Parser Formula
parseVariavel = Var <$> letter

-- Parser para parênteses
parseParentese :: Parser Formula
parseParentese = between (char '(') (char ')') parseImplicacao

-----------------------------------------------------------------
------------ Estrutura de dados para a árvore em si -------------
-----------------------------------------------------------------

-- Tipo para os nós da árvore do tableau
data Node = Node
  { formula :: FormulaVF, -- Rotulado pela fórmula que armazena
  -- (OBS: Uma fórmula com validade false é armazenada como negação)
    branch :: [FormulaVF], -- Todas as fórmulas entre ela e a raiz da árvore.
    -- Não necessário, mais usado para facilitar o processo de verificação de validade
    children :: [Node] -- Os nós filhos desse nó. Pode ser [] se for uma folha.
  }
  deriving (Eq)

-- Overwrite para o show do nó, de forma a mostrar a árvore a partir desse nó de maneira legível e mais facilmente depurável
instance Show Node where
  show :: Node -> String
  show = showNode 0
    where
      showNode :: Int -> Node -> String
      showNode indent (Node f _ children) =
        show indent ++ ": " ++ replicate (indent * 3) ' ' ++ show f ++ showChildren indent children

      showChildren :: Int -> [Node] -> String
      showChildren _ [] = "  [[Folha]]\n"
      showChildren indent children = "\n" ++ concatMap (showNode (indent + 1)) children

-----------------------------------------------------------------
----- Criação da árvore de refutação através de uma fórmula -----
-----------------------------------------------------------------

-- Expande um nó, criando o nó
expandNode :: [FormulaVF] -> [FormulaVF] -> FormulaVF -> Node
expandNode branch backlog f =
  let newBranch = [f] ++ branch
   in if temContradicao newBranch
        then Node f newBranch [] -- Para
        else Node f newBranch (expandChildren newBranch backlog f) -- Continua

-- Expande um nó, criando apenas seus filhos
expandChildren :: [FormulaVF] -> [FormulaVF] -> FormulaVF -> [Node]
-- Casos base: variáveis simples e nenhuma operação no backlog para ser feita
expandChildren _ [] (V (Var _)) = []
expandChildren _ [] (F (Var _)) = []
-- Caso var atômicas e backlog: Coloca as fórmulas do backlog como filhas da atual
expandChildren branch backlog (V (Var _)) = expandChildren branch (tail backlog) (head backlog)
expandChildren branch backlog (F (Var _)) = expandChildren branch (tail backlog) (head backlog)
-- Caso &: variáveis simples e nenhuma operação no backlog para ser feita
expandChildren branch backlog (V (E p q)) = [Node (V p) branch [Node (V q) newBranch (expandChildren (newBranch ++ [(V q)]) (backlog ++ [(V p)]) (V q))]]
  where
    newBranch = branch ++ [V p]
expandChildren branch backlog (F (E p q)) = [expandNode branch backlog (F p), expandNode branch backlog (F q)]
-- Caso ^: cria duas branches
expandChildren branch backlog (V (Ou p q)) = [expandNode branch backlog (V p), expandNode branch backlog (V q)]
expandChildren branch backlog (F (Ou p q)) =  [Node (F p) branch [Node (F q) newBranch (expandChildren (newBranch ++ [(F q)]) (backlog ++ [(F p)]) (F q))]]
  where
    newBranch = branch ++ [F p]
-- Caso ->: cria duas branches, uma com não p e outra com não q
expandChildren branch backlog (V (Imp p q)) = [expandNode branch backlog (F p), expandNode branch backlog (V q)]
expandChildren branch backlog (F (Imp p q)) = [Node (V p) branch [Node (F q) newBranch (expandChildren (newBranch ++ [(F q)]) (backlog ++ [(V p)]) (F q))]]
  where
    newBranch = branch ++ [V p]
-- Casos de negação não atômicas: converte para um dos anteriores,
-- pulando o nó intermediário em si e colocando apenas suas crianças na árvore
expandChildren branch backlog (V (Nao p)) = [expandNode branch backlog (F p)]
expandChildren branch backlog (F (Nao p)) = [expandNode branch backlog (V p)]

-----------------------------------------------------------------
-------- Verificação de se há contradições em uma árvore --------
-----------------------------------------------------------------

-- Dado uma lista de fórmula, verifica se há contradição
temContradicao :: [FormulaVF] -> Bool
temContradicao [] = False
temContradicao (f : fs) = case f of
  V x -> (F x `elem` fs) || temContradicao fs
  F x -> (V x `elem` fs) || temContradicao fs

detectValidade :: [Node] -> Bool
-- Caso base
detectValidade [(Node p branch [])] = temContradicao ([p] ++ branch)
-- Para nó com 1 ou 2 filhos:
detectValidade [(Node _ _ children)] = detectValidade [childF] && detectValidade (tail children)
  where
    childF = head children
detectValidade _ = True -- Caso quando a função é chamado para o [], em por exemplo um nó que tem apenas 1 filho
