import Control.Applicative (liftA2)
import Text.Parsec
import Text.Parsec.Char (char, letter)
import Text.Parsec.String (Parser)
import Data.Traversable (for)

------------------------------------------------------------------
-------------------------- Função Main ---------------------------
------------------------------------------------------------------

-- Main: Le uma fórmula de input e a converte
main :: IO ()
main = do
  -- Lê o arquivo "input.txt"
  input <- readFile "inputs/input1.txt"

  -- Transformar input (string) num datatype (Formula)
  -- utilizando 'case of' para fazer 'pattern matching' no resultado da função 'parseFormula input'

  case parseFormula input of
    Left erro -> print erro
    Right formula -> do
      let tableau = expandNode [] [] (Nao formula)
      putStrLn $ "Arvore tableau para: " ++ show formula
      putStrLn $ show tableau
      putStrLn $ "Tautologia: " ++ show (detectValidade [tableau])

------------------------------------------------------------------
---------- Definição da estrutura de dados da fórmula ------------
------------------------------------------------------------------

-- Tipo fórmula, usado para representar as fórmulas de uma maneira mais fácil de tratar
data Formula
  = Var Char
  | Nao Formula
  | E Formula Formula
  | Ou Formula Formula
  | Imp Formula Formula
  deriving (Eq)

-- Mostra formula como string mais facilmente legível, para facilitar depuração
instance Show Formula where
  show :: Formula -> String
  show (Var c) = [c]
  show (Nao f) = "~" ++ show f
  show (E f1 f2) = "(" ++ show f1 ++ " ^ " ++ show f2 ++ ")"
  show (Ou f1 f2) = "(" ++ show f1 ++ " v " ++ show f2 ++ ")"
  show (Imp f1 f2) = "(" ++ show f1 ++ " -> " ++ show f2 ++ ")"

-----------------------------------------------------------------
--- Função principal para converter uma string em uma fórmula ---
-----------------------------------------------------------------

-- ORDEM DE PRECEDÊNCIA: ¬, ∧, ∨, →, ↔

-- Função principal para converter uma string em uma fórmula
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

-----------------------------------------------------------------
--- Estrutura de dados para a árvore em si                    ---
-----------------------------------------------------------------

-- Tipo para os nós da árvoore do tableau
data Node = Node {
  formula :: Formula,   --Rotulado pela fórmula que armazena
                        --(OBS: Uma fórmula com validade false é arm)

  branch :: [Formula],  --Todas as fórmulas entre ela e a raíz da árvore. 
                        --Não nescessário, mais usado para facilitar o processo de verificação de válidade 
  
  children :: [Node]    --Os nós filhos desse nó. Pode ser [] se for uma folha.
} deriving (Eq)

-- Overwrite para o show do nó, de forma a mostrar a árvore a partir desse nó de maneira legível e mais facilmente depurável
instance Show Node where
  show :: Node -> String
  show  = showNode 0 
    where
      showNode :: Int -> Node -> String
      showNode indent (Node f _ children) =
        show indent ++ ": " ++ replicate (indent * 3) ' ' ++ show f ++ showChildren indent children

      showChildren :: Int -> [Node] -> String
      showChildren _ [] = "  [[Folha]]\n"
      showChildren indent children = "\n" ++ concatMap (showNode (indent + 1)) children 

-----------------------------------------------------------------
--- Criação da árvore de refutação através de uma fórmuka     ---
-----------------------------------------------------------------

-- Expande um nó, criando o nó
expandNode :: [Formula] -> [Formula] -> Formula -> Node
expandNode branch backlog f =
  let newBranch = [f] ++ branch
  in if temContradicao newBranch
     then Node f newBranch [] -- Para
     else Node f newBranch (expandChildren newBranch backlog f) --Continua

--Expande um nó, criando apenas seus filhos
expandChildren :: [Formula] -> [Formula] -> Formula -> [Node]
--Casos base: variáveis simples e nenhuma operação no backlog para ser feita
expandChildren _ [] (Var _) = []
expandChildren _ [] (Nao (Var _)) = []

--Caso var atômicas e backlog: Coloca as fórmulas do backlog como filhas da atual
expandChildren branch backlog (Var _) = expandChildren branch (tail backlog) (head backlog)
expandChildren branch backlog (Nao (Var _)) = expandChildren branch (tail backlog) (head backlog) 
    
--Caso &: variáveis simples e nenhuma operação no backlog para ser feita
expandChildren branch backlog (E p q) = [Node p branch [Node q newBranch (expandChildren (newBranch++[q]) (backlog++[p]) q)]]
                              where newBranch = branch ++ [p]
   
--Caso ^: cria duas branches
expandChildren branch backlog (Ou p q) = [expandNode branch backlog p, expandNode branch backlog q]

--Caso ->: cria duas branches, uma com não p e outra com não q
expandChildren branch backlog (Imp p q) = [expandNode branch backlog (Nao p), expandNode branch backlog q]

--Casos de negação não atômicas: converte para um dois anteriores, 
--pulando o nó intermediario em si e colocando apenas suas crianças na árvore
expandChildren branch backlog (Nao (Nao p)) = [expandNode branch backlog p]
expandChildren branch backlog (Nao (E p q)) = expandChildren branch backlog (Ou (Nao p) (Nao q))
expandChildren branch backlog (Nao (Ou p q)) = expandChildren branch backlog (E (Nao p) (Nao q))
expandChildren branch backlog (Nao (Imp p q)) = expandChildren branch backlog (E p (Nao q))


-----------------------------------------------------------------
--- Verificação de se há contradições em uma árvore           ---
-----------------------------------------------------------------

-- Dado uma lista de fórmula, verifica se há contradição
temContradicao :: [Formula] -> Bool
temContradicao [] = False
temContradicao (f:fs) = (Nao f `elem` fs) || (case f of { Nao x -> x `elem` fs; _ -> False }) || temContradicao fs

detectValidade :: [Node] -> Bool
--Caso base
detectValidade [(Node p branch [])] = temContradicao ([p] ++ branch)
--Para nó com 1 ou 2 filhos:
detectValidade [(Node _ _ children)] = detectValidade [childF] && detectValidade(tail children)
                                                where childF = head children
detectValidade _ = True  --Caso quando a função é chamado para o [], em por exemplo um nó que tem apenas 1 filho
