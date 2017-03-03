implementation module Main

import System.CommandLine
import System.IO
import Control.Monad
import Control.Applicative
import Data.Functor
import StdGeneric

import CustomStdEnv


import Scanner
//import SPLParser
import Error
import Misc


getCl :: IO [String]
getCl = IO getCommandLine

getArgs :: IO [String]
getArgs = fmap (drop 1) getCl

main :: IO ()
main = getArgs										>>= \args.
	let file = hd args in
	readFileM file									>>= \string.
	putStrLn ("contents: \"" +++ string +++ "\"")	>>|
	putStrLn "scanning..."							>>|
	let (tokens, errors) = scanner string in
	putStrLn "Errors:"								>>|
	printL errors									>>|
	putStrLn "Tokens:"								>>|
	printL tokens

printL :: [a] -> IO () | toString a
printL [a:as] = print a >>| (printL as)
printL []	  = return ()

Start w = execIO main w