--Title: Haskell h020
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

		if line.include? ";"
                        string = line.strip
                        num = string.to_i
			outputfile.puts "program \n [ \n fundecls \n [ \n ]"
			outputfile.puts line
		end

		if line.include? ("i") or line.include? ("f")
			string = line.strip
                        num = string.to_i
			outputfile.puts "expr \n [ \n term \n [ \n factor \n [ \n integer_constant \n [ "
			outputfile.puts line
			outputfile.puts " ]\n ]\n ]\n ]\n ]"
		end

mainloop :: Handle -> Handle -> IO ()
mainloop input output =
do ineof <- hIsEOF input
       if ineof
           then return ()
           else do inpStr <- hGetLine input
                   hPutStrLn output (map toUpper inpStr)
                   mainloop input output
