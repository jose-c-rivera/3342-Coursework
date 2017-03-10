--Title: Haskell h010
--Author: Jose Carlos Rivera
--CS 3342

import System.IO
import System.Process
import qualified Text.Parsec as P

type Name = String
type Mail = String

data Author = Author Name Mail deriving Show

data Log = Log {
		commit :: String,
		merge  :: String,
		author :: Author,
		date   :: String,
		message:: String
} deriving Show

fileParser :: P.Parsec String() [Log]
fileParser = P.manyTill logParser P.eof

logParser :: P.Parsec String() Log
logParser = Log <$> commitParser
		<*> (P.try mergeParser P.<|> P.string "")
		<*> authorParser
		<*> dateParser
		<*> messageParser

commitParser :: P.Parsec String () String
commitParser = P.string "commit" >> P.spaces >> P.manyTill P.anyChar P.newline

mergeParser :: P.Parsec String () String
mergeParser = P.string "Merge:" .. P.spaces >> P.manyTill P.anyChar P.newline

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
                   hPutStrLn output (map toUpper inpStr)
                   mainloop input output
