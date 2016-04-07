-- rv-2-rka
-- xmagam00
-- Martin Maga
import Data.Char
import System.IO
import System.Environment
import Control.Monad
import Data.List
import Control.Monad.State
import Data.List.Split
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import Data.String






-- Takes the ParseContext result from the call to parser
-- and converts it to the NFA structure and returns it
convertToNFA str = let context = snd $ parser str
                       transTable = transitions context
                       startNode = last $ nodeList context
                       finalNode = head $ nodeList context
                       valueSet = values context
                   in AutomataMachine {
                                      start = startNode,
                                      table = transTable,
                                      final = Set.singleton finalNode
                                      }


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


type RegexParseState a = State ParseContext a


data AutomataMachine = AutomataMachine{start :: Node,final :: Set Node,table :: [Transition]} deriving (Show, Eq)
                                  


startState = Context [] [] [] 1 Set.empty


concatenationS = (chr 0)


epsilonS = (chr 1)


operatorList = [(concatenationS,7),('*',10),('|',5),('(',1),(')',1)]

operatorFunctions = [(concatenationS,setConcat),('|',setUnion),('*',setStar),('(',setParen),(')',setParen)]

getPrecedence x = fromJust ( lookup x operatorList)
isOperator x = isJust ( lookup x operatorList)
isValue x = not (isOperator x)

parser string = (runState $ do
                mapM_ charProcessing (apendSymbols ("(" ++ string ++ ")"))
                exucuteNOtEmpty) startState


apendSymbols ('(':'|':string) =  '(':epsilonS:apendSymbols ('|':string)
apendSymbols ('|':')':string) =  '|':epsilonS:apendSymbols (')':string)
apendSymbols (q:y:string) =
    if (((isValue q || q == ')' || q == '*') )
      && (isValue y || y == '('))
    then q:concatenationS:apendSymbols (y:string)
    else q : apendSymbols (y:string)
apendSymbols q = q

charProcessing x = do
  if  (isValue x) then inputProcessing x else operatorProcessingOrParen x

inputProcessing x = do
  fromNode <- newNodeCreation
  toNode <- newNodeCreation
  st <- get
  let isEpsilon = x == epsilonS
      getValue x = case isEpsilon of
                   True -> epsilon
                   False-> Just x
      newTrans = (fromNode, toNode, getValue x) : (transitions st)
      newNodes = toNode : fromNode : (nodeList st)
      newValues = case isEpsilon of
                    False -> Set.insert x $ values st
                    True -> values st
  put $ st { nodeList = newNodes, transitions = newTrans, values = newValues}

operatorProcessingOrParen x 
  | x == '(' = queueOp x
  | x == ')' = exucuteIfBracket
  | True = operatorProcessing x

operatorProcessing x = do
  precQ <- precendeQueue
  if (precQ < (getPrecedence x)) then
      queueOp x else
      queteOperatorExecution >> operatorProcessing x

concatOperator = do
  ops <- gets operators
  if  null ops then 
    return  Nothing
  else 
    return (Just (head ops))


precendeQueue = do
    op <- concatOperator
    maybe (return 0) (return.getPrecedence) op

exucuteNOtEmpty = do
  mop <- concatOperator
  case mop of
    Just op -> execOp op >> exucuteNOtEmpty
    Nothing -> return ()


exucuteIfBracket = do
  mop <- concatOperator
  case mop of
    Just '(' -> execOp '('
    Just op -> execOp op >> exucuteIfBracket
    Nothing -> return ()



queteOperatorExecution = do
  op <- concatOperator
  execOp $ fromJust op


execOp op = do
    (fromJust $ lookup op operatorFunctions)

-- Queue an operator to be execute later
queueOp x = do
  st <- get
  let newOps = x : (operators st )
  put $ st { operators = newOps }

newNodeCreation = do
  st <- get
  let newNode = nextNode st
      newNext = newNode + 1
  put $ st { nextNode = newNext }
  return newNode


setConcat = do
  st <- get
  let nodes = nodeList st
      newNodes = (nodes !! 0) : (nodes !! 3) : (drop 4  nodes)
      newTransitions = transitions st ++ [(nodes !! 2, nodes !! 1, epsilon)]
      newOperators = tail $ operators st
  put $ st { nodeList = newNodes,
             transitions = newTransitions ,
             operators = newOperators}


setUnion = do
  fromNode <- newNodeCreation
  toNode <- newNodeCreation
  st <- get
  let nodes = nodeList st
      newNodes = toNode : fromNode  : (drop 4  nodes)
      newTransitions = transitions st ++
                       [(fromNode, nodes !! 1, epsilon),
                        (fromNode, nodes !! 3, epsilon),
                        (nodes !! 2, toNode, epsilon),
                        (nodes !! 0, toNode, epsilon)]
      newOperators = tail $ operators st
  put $ st { nodeList = newNodes,
             transitions = newTransitions ,
             operators = newOperators}



setStar = do
  fromNode <- newNodeCreation
  toNode <- newNodeCreation
  string <- get
  let nodes = nodeList string
      newNodes = (toNode) : fromNode  : (drop 2  nodes)
      newTransitions = transitions string ++
                       [(fromNode, nodes !! 1, epsilon),
                        (fromNode, nodes !! 0, epsilon), 
                        (nodes !! 0, toNode, epsilon),
                        (toNode,fromNode,epsilon)]
      newOperators = tail $ operators string
  put (string {nodeList = newNodes, transitions = newTransitions,operators = newOperators})

setParen = modify (\string-> string { operators = tail (operators string )})


getContent :: [FilePath] -> IO String
getContent args = do
                if length args == 2 then do
                    content <- readFile (args !! 1)
                    return content
                else do
                    content <- getLine
                    return content


shunt o p [] = (reverse o) ++ p
shunt o [] (x:xs)
    | x `elem` [")", "/", "("] = shunt o [x] xs
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



toPostfix = (intercalate " " . shunt [] [] . words)


toInfix xs = xs

generateAutomataStates:: Int->[Int]
generateAutomataStates endState = [ states | states <- [1..endState]] 


changeOrder xs = do
                  let firstIndex =  (','  `elemIndices` xs) 
                  let (firstState,rest1) = splitAt (firstIndex !! 0) xs
                  let (secondState,rest2) = splitAt ((','  `elemIndices` tail rest1) !! 0) (tail rest1)
                  let output = firstState ++"," ++  (changeState (tail rest2)) ++"," ++  secondState
                  putStrLn output
                   
                     
changeState xs = if (xs == "Nothing") then "" else tail(snd(splitAt 4 xs))
                     

processOutput xs = do
                let automataResult =  dropWhile  (/='=') (takeWhile (/='}')(dropWhile (=='{') (tail (dropWhile (/=' ')(show ((convertToNFA xs)))))))
                let (startState, rest) = splitAt 1 (dropWhile (==' ') (dropWhile  (/=' ') automataResult))
                -- end string processing
                let end = drop 20 rest
                --get index of ]
                let endIndex =  (']'  `elemIndices` end) !! 0
                let (endState, rest2) = splitAt endIndex end

                -- get all states
                let allAutomataStates =  map show (generateAutomataStates (read endState))
                
                let rulesIndex = ('['  `elemIndices` rest2) !! 0
                let (rest3, rules) = splitAt rulesIndex rest2 

                --print all states
                putStrLn (intercalate "," allAutomataStates)
                --print start state
                putStrLn startState
                --print end state
                putStrLn endState
               
                mapM_ changeOrder ((filter (not . null)  (map (filter (/='(')) (map tail (splitOn ")" (filter (/='\'') (filter (/='*') ( "(" ++ ( (tail (rules)))))))))))


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
          processOutput content