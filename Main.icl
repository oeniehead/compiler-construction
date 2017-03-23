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
import PrettyPrinter


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
	print "Tokens:"								>>|
	printAll tokens								>>|
	print "Parsing..."							>>|
	let (syntaxTrees, pErrors) = parser tokens in
	(if (not (isEmpty pErrors))
		(	print "Parser Errors:" >>| printAll pErrors)
		(	return ())
	)											>>|
	(if (not (isEmpty syntaxTrees))
		(	let result = hd syntaxTrees in
			print "first syntax tree"		>>|
			printAll (fst result)			>>|
			print "tokens left:"			>>|
			printAll (snd result)			>>|
			print "Pretty print:"			>>|
			print (prettyPrint (fst result))	)
		(	print "No syntax trees"				)
	)											>>|
	print ("Nr of derivations: " +++ (toString (length syntaxTrees))) >>|
	
	print "end"

import GenString
import StdOverloaded
derive gString Decl, FunDecl, VarDecl, Type, Stmt, Maybe, Expr, BasicType, FunCall,
	IdWithFields, UnOp, BinOp, Field

instance toString Decl where toString d = gString{|*|} d

printAll :: [a] -> IO () | toString a
printAll [a:as] = print a >>| (printAll as)
printAll []	  = return ()


Start w = execIO main w