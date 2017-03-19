--Title: Haskell h030
--Author: Jose Carlos Rivera
--Purpose: Build a parser from the output of scanner from h010.hs
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
