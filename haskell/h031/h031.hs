--Title: Haskell h031
--Author: Jose Carlos Rivera
--Purpose: Interpreter and continuation of h030
--CS 3342

import System.IO
import System.Process


main :: IO()
main = do


input <- openFile "input.txt" ReadMode
output <- openFile "output.txt" WriteMode

mainloop input output
hClose input
hClose output

mainloop :: Handle -> Handle -> IO ()
mainloop input output =
do ineof <- hIsEOF input
       if ineof
           then return ()
           else do inpStr <- hGetLine input
		   putStrLn output ("Test parser");
                   hPutStrLn output (map toUpper inpStr)
                   mainloop input output
