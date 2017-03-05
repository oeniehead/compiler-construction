implementation module Main

import System.CommandLine
import System.IO
import Control.Monad
import Control.Applicative
import Data.Functor
import StdGeneric

import CustomStdEnv


import Scanner
import SPLParser
import Error
import Misc


getCl :: IO [String]
getCl = IO getCommandLine

getArgs :: IO [String]
getArgs = fmap (drop 1) getCl

main :: IO ()
main =
	print "=========================="			>>|
	getArgs										>>= \args.
	let file = hd args in
	readFileM file								>>= \string.
	print ("contents: \"" +++ string +++ "\"")	>>|
//	print "scanning..."							>>|
	let (tokens, sErrors) = scanner string in
	(if (not (isEmpty sErrors))
		(	print "Scanner errors:")
		(	return ())
	)											>>|
	printAll sErrors							>>|
//	print "Tokens:"								>>|
//	printAll tokens								>>|
//	print "Parsing..."							>>|
	let (syntaxTrees, pErrors) = parser tokens in
	(if (not (isEmpty pErrors))
		(	print "Parser Errors:")
		(	return ())
	)											>>|
	printAll pErrors							>>|
	print ("Nr of derivations: " +++ (toString (length syntaxTrees)))
	

printAll :: [a] -> IO () | toString a
printAll [a:as] = print a >>| (printAll as)
printAll []	  = return ()

Start w = execIO main w