implementation module Main

import System.CommandLine
import System.IO
import Control.Monad
import Control.Applicative
import Data.Functor
import StdGeneric

import CustomStdEnv


import Scanner
import Error
import Misc


getCl :: IO [String]
getCl = IO getCommandLine

getArgs :: IO [String]
getArgs = fmap (drop 1) getCl

main :: IO ()
main = getArgs									>>= \args.
//	let file = hd args in
	let file = "test" in
	putStrLn ("file name: {" +++ file +++ "}")	>>|
	readFileM file								>>= \string.
	putStrLn ("contents: {" +++ string +++ "}")	>>|
	let (tokens, errors) = scan string in
	printL tokens								>>|
	printL errors

scan :: !String -> !(![Token], ![Error])
scan s = scanner s

printL :: [a] -> IO () | toString a
printL [a:as] = print a >>| (printL as)
printL []	  = return ()

//Start w = execIO main w

Start = scanner "c"