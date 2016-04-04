-- rv-2-rka
-- xmagam00
-- Martin Maga

import System.IO
import System.Environment
import Control.Monad
import Data.List
import Data.Char
import Control.Monad.State
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set



-- Structure which represent a finite autamata
-- All you need to represent a NFA is the start node, final node,
-- a table of transitions and the set of all values we transition on
data FiniteMachine = FiniteMachine{  start :: Node,
                                     final :: Set Node,
                                     table :: [Transition],
                                     alphabet :: Set Char
                                  } deriving (Show, Eq)



-- Takes the ParseContext result from the call to parseRegex
-- and converts it to the NFA structure and returns it
convertToNFA str = let context = snd $ parseRegex str
                       transTable = transitions context
                       startNode = last $ nodeList context
                       finalNode = head $ nodeList context
                       valueSet = values context
                   in FiniteMachine {
                                      start = startNode,
                                      table = transTable,
                                      final = Set.singleton finalNode,
                                      alphabet = valueSet}


-- NFA node
type Node = Integer

-- The value for an edge in a NFA
type TransitionValue = Maybe Char

-- A transition in a NFA is a tuple of
-- StartNode , DestinationNode, Value to transition on
type Transition = (Node,Node,TransitionValue)

-- The value of the edge in the NFA is a Maybe Char
-- Where Nothing is the epsilon transition
-- therefore lets just rename Nothing to epsilon
epsilon = Nothing


-- Represent the list of pending operators
-- Every new operator seen will be put at the front of the list
type OperatorList = [Char]

-- The state that gets passed around which we used to build up the NFA
data ParseContext = Context
                    {
                      nodeList :: [Node],
                      transitions :: [Transition],
                      operators :: OperatorList,
                      nextNode :: Node,
                      values :: Set Char
                    } deriving (Show, Eq)

-- Alias the State data constructor with a more friendly name
type RegexParseState a = State ParseContext a

-- The intial state of the parser
initialContext = Context [] [] [] 1 Set.empty

-- Set the symbol for the concat symbol to be some symbol that won't appear in the regex
concatSymbol = chr 0

-- Set the symbol for the epsilon symbol to be some symbol that won't appear in the regex
epsilonSymbol = chr 1

-- The list of operators with their associated precedences
operatorList =
    [(concatSymbol,7),
     ('*',10),
     ('|',5),
     ('(',1),
     (')',1)]

-- List of operators with their associated functions
operatorFunctions =
    [(concatSymbol,doConcat),
     ('|',doUnion),
     ('*',doStar),
     ('(',doParen),
     (')',doParen)]


getPrecedence x = fromJust $ lookup x operatorList
isOperator x = isJust $ lookup x operatorList
isValue x = not $ isOperator x


-- Run the parser over a string
parseRegex str = (runState $
              do
                mapM_ processChar (appendConcatsAndEpsilons ("(" ++ str ++ ")"))
                executeOperatorsUntilEmpty) initialContext


-- Process the regex input string and
-- add concatSymbol where a concat operation implicitly should be
-- add epsilonSymbol where we see we are doing a union on epsilon
-- This makes the rest of the code more general instead of putting
-- special cases into it
appendConcatsAndEpsilons ('(':'|':str) =  '(':epsilonSymbol:appendConcatsAndEpsilons ('|':str)
appendConcatsAndEpsilons ('|':')':str) =  '|':epsilonSymbol:appendConcatsAndEpsilons (')':str)
appendConcatsAndEpsilons (x:y:str) =
    if ((isValue x || x == ')' || x == '*') && (isValue y || y == '('))
    then x:concatSymbol:appendConcatsAndEpsilons (y:str)
    else x : appendConcatsAndEpsilons (y:str)
appendConcatsAndEpsilons x = x


-- Process a character from the input string
-- Decides if it is a operator or not and calls the corresponding method
processChar x = do
  case isValue x of
    True -> processInput x
    False -> processOperatorOrParen x


-- When the next character is not an operator we create a transition which
-- represents this character by creating two NFA nodes and adding a transition
-- on that character between them
processInput x = do
  nodeFrom <- createNewNode
  nodeTo <- createNewNode
  st <- get
  let isEpsilon = x == epsilonSymbol
      getValue x = case isEpsilon of
                   True -> epsilon
                   False-> Just x
      newTrans = (nodeFrom, nodeTo, getValue x) : (transitions st)
      newNodes = nodeTo : nodeFrom : (nodeList st)
      newValues = case isEpsilon of
                    False -> Set.insert x $ values st
                    True -> values st
  put $ st { nodeList = newNodes, transitions = newTrans, values = newValues}


-- Either process close paren or operator
processOperatorOrParen x = do
  case x of
    ')' -> executeUntilOpenParenthesis
    '(' -> queueOperator x
    otherwise -> processOperator x

-- Compare the current operator with the operator on the front of the operator list
-- If the current operator has a higher precedence append it to the list
-- otherwise execute the operator at the head of the list and the repeat
-- this function
processOperator x = do
  precQ <- queuedPrecedence
  if (precQ < (getPrecedence x)) then
      queueOperator x else
      executeQueuedOperator >> processOperator x

-- Get the head of the operator list wrapped in a Maybe monad
-- This is used incase the list is empty so we can return Nothing
peekOperator = do
  ops <- gets operators
  case null ops of
    True -> return $ Nothing
    False -> return $ Just (head ops)

-- Get the precedence of the operator at the head of the operator list
-- If the operator list is empty it returns a precedence of 0
-- This will ensure that a comparison of any operator with an empty list
-- results in the other operator being pushed on the list
queuedPrecedence = do
    op <- peekOperator
    maybe (return 0) (return.getPrecedence)  op


-- Execute every operator on the stack
-- This is used at the end of the parsing
executeOperatorsUntilEmpty = do
  mop <- peekOperator
  case mop of
    Just op -> executeOperator op >> executeOperatorsUntilEmpty
    Nothing -> return ()



-- Execute operators until and including open parenthesis
executeUntilOpenParenthesis = do
  mop <- peekOperator
  case mop of
    Just '(' -> executeOperator '('
    Just op -> executeOperator op >> executeUntilOpenParenthesis
    Nothing -> return ()


-- Execute the function which corresponds to the operator which
-- is at the head of the operator list
executeQueuedOperator = do
  op <- peekOperator
  executeOperator $ fromJust op

-- Given an operator execute its corresponding function
executeOperator :: Char -> RegexParseState ()
executeOperator op = do
    (fromJust $ lookup op operatorFunctions)

-- Queue an operator to be execute later
queueOperator x = do
  st <- get
  let newOps = x : (operators st )
  put $ st { operators = newOps }



-- Get the next NFA Node and then update the state
createNewNode :: RegexParseState Node
createNewNode = do
  st <- get
  let newNode = nextNode st
      newNext = newNode + 1
  put $ st { nextNode = newNext }
  return newNode


-- Execute the concat operator
doConcat :: RegexParseState ()
doConcat = do
  st <- get
  let nodes = nodeList st
      newNodes = (nodes !! 0) : (nodes !! 3) : (drop 4  nodes)
      newTransitions = transitions st ++ [(nodes !! 2, nodes !! 1, epsilon)]
      newOperators = tail $ operators st
  put $ st { nodeList = newNodes,
             transitions = newTransitions ,
             operators = newOperators}


-- Execute the union operator
doUnion :: RegexParseState ()
doUnion = do
  nodeFrom <- createNewNode
  nodeTo <- createNewNode
  st <- get
  let nodes = nodeList st
      newNodes = nodeTo : nodeFrom  : (drop 4  nodes)
      newTransitions = transitions st ++
                       [(nodeFrom, nodes !! 1, epsilon),
                        (nodeFrom, nodes !! 3, epsilon),
                        (nodes !! 2, nodeTo, epsilon),
                        (nodes !! 0, nodeTo, epsilon)]
      newOperators = tail $ operators st
  put $ st { nodeList = newNodes,
             transitions = newTransitions ,
             operators = newOperators}


-- Execute the star (closure) operator
doStar :: RegexParseState ()
doStar = do
  nodeFrom <- createNewNode
  nodeTo <- createNewNode
  st <- get
  let nodes = nodeList st
      newNodes = nodeTo : nodeFrom  : (drop 2  nodes)
      newTransitions = transitions st ++
                       [(nodeFrom, nodes !! 1, epsilon),
                        (nodeFrom, nodes !! 0, epsilon), -- skip over transition since * could mean 0 times
                        (nodes !! 0, nodeTo, epsilon),
                        (nodeTo,nodeFrom,epsilon)]
      newOperators = tail $ operators st
  put $ st { nodeList = newNodes,
             transitions = newTransitions ,
             operators = newOperators}

-- For a parenthesis we just remove it from the operator list
doParen :: RegexParseState ()
doParen = do
  modify $  \st-> st { operators = tail $ operators st }





getContent :: [FilePath] -> IO String
getContent args = do
                if length args == 2 then do
                    content <- readFile (args !! 1)
                    return content
                else do
                    content <- getLine
                    return content
shunt :: [String] -> [String] -> [String] -> [String]
shunt o p [] = (reverse o) ++ p
shunt o [] (x:xs)
    | x `elem` [")", "/", "("] = shunt o [x] xs
    | x == ")" = error "mismatched parenthesis"
    | otherwise = shunt (x:o) [] xs
shunt o (p:ps) (x:xs)
    | x == "(" = shunt o (x:p:ps) xs
    | x == ")" = case (span (/= "(") (p:ps)) of
        (as, b:bs) -> shunt (as ++ o) bs xs
    | x == "*" = shunt o (x:p:ps) xs
    | x `elem` ["."] = case (p) of
        "*" -> shunt (p:o) (x:ps) xs
        "(" -> shunt o (x:p:ps) xs
        otherwise -> shunt (p:o) (x:ps) xs
    | otherwise = shunt (x:o) (p:ps) xs



-- method for postfix execution
toPostfix:: [Char]->[Char]
toPostfix = (intercalate " " . shunt [] [] . words)


-- method for infix execution
toInfix:: [Char]->[Char]
toInfix xs = xs


-- main code execution
main = do
     args <- getArgs
     content <- getContent args


     if args !! 0 == "-r" then do
         putStrLn (filter (/=' ') (toPostfix (toInfix (intersperse ' ' (content)))))
     else
       --empty input it means epsilon
        if length content == 0 then do
            putStrLn "1,2"
            putStrLn "1"
            putStrLn "2"
            putStrLn "1,,2"
        else do
            putStrLn (show (convertToNFA content))

