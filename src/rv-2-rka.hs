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

-- |convertToNfa take regular expression as string and parse it
convertToNFA str = let context= snd((parser str))
                       transTable= (prechody context)
                       startUzol= last(nodesOfList context)
                       finalUzol= head(nodesOfList context)
                       valueSet= (hodnoty context)
                   in AutomataMachine {stateOfStart = startUzol, tabulka = transTable, final = Set.singleton finalUzol}

-- | Typ uzol predstavuje uzol binarneho stromu
type Uzol = Integer

-- | Typ 'ValuePrechod' Predstavuje prechodovy element zo stavu do stavu
type ValuePrechod = Maybe Char

-- | 'Prechod' predstavune 3 uzol,uzol, prechod
type Prechod = (Uzol,Uzol,ValuePrechod)

-- | Hodnota epsilonu je definovana ako Nothing
epsilon = Nothing


-- | OperatorsOfList predstavuje list operacii
type OperatorsOfList = [Char]


data ObsahParse =Obsah
                    {nodesOfList::[Uzol],  -- | NodeOfList predstavuje zoznam vsetkych uzol stromu
                      prechody::[Prechod], -- | Prechody, je zoznam vsetkych prechodov
                      operators::OperatorsOfList, -- | OperatorsOfList je zoznam vsetkych operatorv
                      nextUzol::Uzol, -- | Uzol
                      hodnoty::Set(Char) -- | Mnozina hodnot
                    }deriving(Show,Eq)

-- | ParseState predsavuje aktulne spracovany stav
type ParseState a=State ObsahParse a

-- | AutomataMachine predstavuje 
data AutomataMachine=AutomataMachine{
  stateOfStart::Uzol, -- | Pociatocny stav
  final::Set Uzol, -- | Koncovy stav
  tabulka::[Prechod] -- | Tabulka prechodov
  }deriving(Show,Eq)

-- | Pociatocny stav empty list                                  
emptyList = []

-- | Pociatocny stav prazdny set
emptySet = Set.empty

-- | Start state
startState =Obsah (emptyList) (emptyList) (emptyList) (1) (emptySet)

-- | Concatenacny symbol
concatenationS=(chr 0)
-- | Epsilon znak
epsilonS=(chr 1)

-- | Zoznam precedencii operatorov
precendeList = [1,5,7,10]

-- | Zoznam operatorov
operatorList = [')','(','|','*']

-- | List operatorov
opList = [(operatorList !! 0,precendeList !! 0),(operatorList !! 2,precendeList !! 1),(operatorList !! 1, precendeList !! 0),(operatorList !! 3, precendeList !! 3),(concatenationS, precendeList !! 2)]

-- | Zoznam fukcii
opFuncs = [(operatorList !! 3,setStar),(operatorList !! 1,setParen),(operatorList !! 0,setParen),(concatenationS,setConcat),(operatorList !! 2,setUnion)]

-- | ziskat precendiu
gainPrecende x =(fromJust(lookup x opList))

-- | funkcia parser
parser string = (runState(do -- | spracuje vsetky znak
                mapM_ charProcessing(symbolAppendder( "("++string++")"))
                exucuteNOtEmpty))startState

-- | Funkcia appenduje symboly
symbolAppendder ('(':'|':string)=('(':epsilonS:symbolAppendder ('|':string))
symbolAppendder ('|':')':string)=('|':epsilonS:symbolAppendder (')':string))
symbolAppendder (q:y:string)=
    if (((isVal q || q == ')' || q == '*') )
      && (isVal y || y == '('))
    then q:concatenationS:symbolAppendder (y:string)
    else q : symbolAppendder (y:string)
symbolAppendder q = q

-- | spracovanie znakov v regex
charProcessing y
  | (isVal y) = inputProcessing y
  | True =  operatorProcessingOrParen y


--
inputProcessing x = do
  fromUzol <- newUzolCreation
  toUzol <- newUzolCreation
  st <- get
  let isEpsilon=(x == epsilonS)
      getValue x = case isEpsilon of
                   True -> epsilon
                   False-> Just x
      newTrans=(fromUzol, toUzol, getValue x) : (prechody st)
      newUzols= toUzol : fromUzol : (nodesOfList st)
      newValues= case isEpsilon of
                    False -> Set.insert x (hodnoty st)
                    True -> hodnoty st
  put(st{nodesOfList=newUzols,prechody=newTrans,hodnoty=newValues})


-- | Spracovanie operatoru *
operatorProcessingOrParen x 
  | x == '(' = queueOp x
  | x == ')' = exucuteIfBracket
  | True = operatorProcessing x

-- | Spracovanie operatorov
operatorProcessing x = do
  precQ <- precendeQueue
  if (precQ < (gainPrecende x)) then
      queueOp x else
      queteOperatorExecution >> operatorProcessing x

isOp x =isJust ( lookup x opList)
isVal x =not (isOp x)

concatOperator = do
  ops <- gets operators
  if  null ops then 
    return  Nothing
  else 
    return (Just (head ops))


precendeQueue = do
    op <- concatOperator
    maybe (return 0) (return.gainPrecende) op

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
    (fromJust $ lookup op opFuncs)

-- Queue an operator to be execute later
queueOp x = do
  st <- get
  let newOps = x : (operators st )
  put $ st { operators = newOps }

newUzolCreation = do
  st <- get
  let newUzol = nextUzol st
      newNext = newUzol + 1
  put $ st { nextUzol = newNext }
  return newUzol


setConcat = do
  st <- get
  let nodes = nodesOfList st
      newUzols = (nodes !! 0) : (nodes !! 3) : (drop 4  nodes)
      newPrechods = prechody st ++ [(nodes !! 2, nodes !! 1, epsilon)]
      newOperators = tail $ operators st
  put $ st { nodesOfList = newUzols,
             prechody = newPrechods ,
             operators = newOperators}


setUnion = do
  fromUzol <- newUzolCreation
  toUzol <- newUzolCreation
  st <- get
  let nodes = nodesOfList st
      newUzols = toUzol : fromUzol  : (drop 4  nodes)
      newPrechods = prechody st ++
                       [(fromUzol, nodes !! 1, epsilon),
                        (fromUzol, nodes !! 3, epsilon),
                        (nodes !! 2, toUzol, epsilon),
                        (nodes !! 0, toUzol, epsilon)]
      newOperators = tail $ operators st
  put $ st { nodesOfList = newUzols,
             prechody = newPrechods ,
             operators = newOperators}



setStar = do
  fromUzol <- newUzolCreation
  toUzol <- newUzolCreation
  string <- get
  let nodes = nodesOfList string
      newUzols = (toUzol) : fromUzol  : (drop 2  nodes)
      newPrechods = prechody string ++
                       [(fromUzol, nodes !! 1, epsilon),
                        (fromUzol, nodes !! 0, epsilon), 
                        (nodes !! 0, toUzol, epsilon),
                        (toUzol,fromUzol,epsilon)]
      newOperators = tail $ operators string
  put (string {nodesOfList = newUzols, prechody = newPrechods,operators = newOperators})

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
    | x `elem` ["/", "/", "-"] = shunt o [x] xs
    | otherwise = shunt (x:o) [] xs


shunt o (p:ps) (x:xs)
    | x == "*" = shunt o (x:p:ps) xs
    | x `elem` ["."] = case (p) of
        "*" -> shunt (p:o) (x:ps) xs
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
                let ciarkaIndeces = (','  `elemIndices` automataResult) !! 0
                let (startState, rest) = splitAt (ciarkaIndeces -2) (dropWhile (==' ') (dropWhile  (/=' ') automataResult))
                -- end string processing
                let endIndex2 =  ('['  `elemIndices` rest) !! 0

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
                --print stateOfStart state
                putStrLn startState
                --print end state
                putStrLn endState
               
                mapM_ changeOrder ((filter (not . null)  (map (filter (/='(')) (map tail (splitOn ")" (filter (/='\'') (filter (/='*') ( "(" ++ ( (tail (rules)))))))))))



data Expression = Const String | Exp Expression String Expression
 
precedence :: Expression -> Int
precedence (Const _) = 4
precedence (Exp _ op _)
  | op `elem` ["*"] = 3
  | op `elem` ["+","."] = 2
  | otherwise = 0
 
leftAssoc :: Expression -> Bool
leftAssoc (Const _) = False
leftAssoc (Exp _ op _) = op `notElem` ["*",".","+"]
 
rightAssoc :: Expression -> Bool
rightAssoc (Const _) = False
rightAssoc (Exp _ op _) = op `elem` ["*"]
 
instance Show Expression where
  show (Const x) = x
  show exp@(Exp l op r) = left++" "++op++" "++right
    where left  = if leftNeedParen then "( "++(show l)++" )" else show l
          right = if rightNeedParen then "( "++(show r)++" )" else show r
          leftNeedParen = (leftPrec < opPrec) || ((leftPrec == opPrec) && (rightAssoc exp))
          rightNeedParen = (rightPrec < opPrec) || ((rightPrec == opPrec) && (leftAssoc exp))
          leftPrec  = precedence l
          rightPrec = precedence r
          opPrec    = precedence exp
 
buildExp :: [Expression] -> String -> [Expression]
buildExp stack x
  | not.isOp $ x = Const x : stack
  | otherwise    = Exp l x r : rest
    where r:l:rest = stack
          isOp = (`elem` ["*","+","."])

replaceO [] = []
replaceO (x:xs) = 
     if x == '+' 
     then '|' : replaceO xs 
     else x : replaceO xs

replace1 [] = []
replace1 (x:xs) = 
     if x == '.' 
     then ' ' : replace1 xs 
     else x : replace1 xs

insertAfter [] = []
insertAfter (x:xs)
  | x == '*' = ',':x:insertAfter xs
  | True = x:insertAfter xs

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
          let postFixRegex = filter (/=',') $ filter (/=' ') (replace1 $ replaceO $ filter (/=' ')(show $ ((head.(foldl buildExp []).words) (intersperse ' ' $ insertAfter content))))
          processOutput postFixRegex