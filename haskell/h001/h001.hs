--Title: Haskell Basics and Practice
--Author: Jose Carlos Rivera
--CS 3342

import System.IO
import System.Process

main = do

       --Redirect command to program without showing output to user
       logFile <- readProcess "git" ["log"] ""
       writeFile "logout.txt"(logFile)
