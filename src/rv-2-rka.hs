module Main where
import System.Environment
  
main = do
        args <- getArgs
        if length args /= 1
                then error "just one argument!"
                else do
                        let fileName = head args
                        input <- readFile fileName
                        putStrLn fileName

--test =  do
--         input <- readFile "/home/rdy/haskell/workspace/flp-basic/myTests/basic.test"
--         putStrLn $ toLowerCase (replace (replace (replaceReturnInScope $ removeLineComments $ removeLongComments input++"\n") '\n' "${newline}") '\r' "${newLine}")
--             
