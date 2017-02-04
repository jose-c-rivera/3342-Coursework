--Title: Haskell Basics and Practice
--Author: Jose Carlos Rivera
--CS 3342

import System.IO
import System.Process


main = do

       --Simple OPEN/PRINT of got log output
       test <- readProcess "git" ["log"] ""
       putStrLn test
