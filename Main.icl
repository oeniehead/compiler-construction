implementation module Main

import System.CommandLine
import System.IO
import Control.Monad
import Control.Applicative
import Data.Functor
import Data.List
import StdGeneric
import GenString

import CustomStdEnv

import Scanner
import AST
import Parser
import Error
import Misc
import PrettyPrinter
import Data.Either
import BindingAnalysis
import CodeGenerator


getCl :: IO [String]
getCl = IO getCommandLine

getArgs :: IO [String]
getArgs = fmap (drop 1) getCl

main :: IO ()
main =
	print "=========================="			>>|
	getArgs										>>= \args.
	let file = hd args in
	print ("File: " +++ file)					>>|
	readFileM file								>>= \string.
	print ("contents: \"" +++ string +++ "\"")	>>|
	print "scanning..."							>>|
	let (tokens, sErrors) = scanner string in
	(if (not (isEmpty sErrors))
		(	print "Scanner errors:")
		(	return ())
	)											>>|
	printAll sErrors							>>|
//	print "Tokens:"								>>|
//	printAll tokens								>>|
	print "Parsing..."							>>|
	case parser tokens of
		(Left pErrors) = print "Parser Errors:" >>| printAll pErrors
		(Right ast)
				=
					print "Syntax tree"			>>|
					printAll ast				>>|
					print "Pretty print:"		>>|
					print (prettyPrint ast)		>>|
					print "Binding:"			>>|
					return (doBindingAnalysis ast) >>= \result.
					case result of
						(Left bErrors) = print "Binding Errors:" >>| printAll bErrors
						(Right ast)		= print (prettyPrint ast)
	
					
					
	

import GenString
import StdOverloaded

instance toString Decl where toString d = gString{|*|} d

printAll :: [a] -> IO () | toString a
printAll [a:as] = print a >>| (printAll as)
printAll []	  = return ()


Start w = execIO main w