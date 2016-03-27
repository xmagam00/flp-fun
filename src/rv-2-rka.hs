import System.IO
import System.Environment
import Data.List.Split
  
addSpace :: String -> String
addSpace = intercalate " " . chunksOf 1

solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "." = [x] ++ "." ++  [y] ++ ys  
            foldingFunction (x:y:ys) "+" = "(" ++ [x] ++ [y] ++ ")" ++ ys  
            foldingFunction (x:y:ys) "*" = [x] ++ "*" ++ ys  
            foldingFunction xs numberString = read numberString ++ xs  

main = do
        args <- getArgs
        if (length args == 2) 
            handle <- openFile (args !! 1) ReadMode
            content <- hGetsContent handle
            hClose handle
        else
            content <- getLine
        
        contentWithSpaces <- addSpace content
        
        regular <- solveRPN contentWithSpaces
        
        #write regular expression
        if (args !! 0 == "-r")
           putStrLn regular
        else
            #create automata
        
        
        
            
            
        
	
	
	
	
	
	
	
	
		
