--Title: Haskell Basics and Practice
--Author: Jose Carlos Rivera
--CS 3342

import System.IO
import System.Process
import qualified Text.Parsec as P

type NAame = String
type Mail = String

data Author = Author Name Mail deriving Show

data Log = Log {
                commit :: String,
                merge  :: String,
                author :: Author,
                date   :: String,
                message :: String
} deriving Show

fileParser :: P.Parsec String () [Log]
fileParser = P.manyTill logParser P.eof

logParser :: P.Parsec String () Log
logParser = Log <$> commitParser
		<*> (P.try mergeParser P.<|> P.string "")
		<*> authorParser
		<*> dateParser
		<*> messageParser

commitParser :: P.Parsec String () String
commitParser = P.string "commit" >> P.spaces >> P.manyTill P.anyChar P.newline

main :: IO()
main = do

       --Redirect command to program without showing output to user
      	logFile <- readProcess "git" ["log"] ""
      	writeFile "logout.txt"(logFile)
	case P.parse fileParser "" logs of 
		Left 1 -> print 1
		Right r -> print $ show(length r) ++ " commit logs found,"
		
	--ALlow for looking at report of just one task instead of whole report

