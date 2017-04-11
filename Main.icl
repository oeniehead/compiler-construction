implementation module Main

import System.CommandLine
import System.IO
import Control.Monad
import Control.Applicative
import Data.Functor
import StdGeneric
import GenString

import CustomStdEnv


import Scanner
import SPLParser
import Error
import Misc
import PrettyPrinter
import Data.Either
import BindingAnalysis


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
						(Right bErrors) = print "Binding Errors:" >>| printAll bErrors
						(Left ast)		= print (prettyPrint ast)
					
					
	

import GenString
import StdOverloaded
derive gString Decl, FunDecl, VarDecl, Type, Stmt, Maybe, Expr, BasicType, FunCall,
	IdWithFields, UnOp, BinOp, Field, MetaData

instance toString Decl where toString d = gString{|*|} d

printAll :: [a] -> IO () | toString a
printAll [a:as] = print a >>| (printAll as)
printAll []	  = return ()


Start w = execIO main w