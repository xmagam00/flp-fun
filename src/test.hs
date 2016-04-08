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

data Expression = Const String | Exp Expression String Expression
 
precedence :: Expression -> Int
precedence (Const _) = 5
precedence (Exp _ op _)
	| op `elem` ["^"]     = 4
	| op `elem` ["*"] = 3
	| op `elem` ["+","."] = 2
	| otherwise = 0
 
leftAssoc :: Expression -> Bool
leftAssoc (Const _) = False
leftAssoc (Exp _ op _) = op `notElem` ["^","*","+"]
 
rightAssoc :: Expression -> Bool
rightAssoc (Const _) = False
rightAssoc (Exp _ op _) = op `elem` ["^"]
 
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
		      isOp = (`elem` ["^","*","+","."])

replaceO [] = []
replaceO (x:xs) = 
     if x == '+' 
     then '|' : replaceO xs 
     else x : replaceO xs

main = do
	contents <- getLine
	let test = replaceO $ filter (/=' ')(show $ ((head.(foldl buildExp []).words) (intersperse ' ' contents)))
	putStrLn test
