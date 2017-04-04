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
import Data.Either


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
					printAll ast					>>|
					print "Pretty print:"			>>|
					print (prettyPrint ast)

import GenString
import StdOverloaded
derive gString Decl, FunDecl, VarDecl, Type, Stmt, Maybe, Expr, BasicType, FunCall,
	IdWithFields, UnOp, BinOp, Field, MetaData

instance toString Decl where toString d = gString{|*|} d

printAll :: [a] -> IO () | toString a
printAll [a:as] = print a >>| (printAll as)
printAll []	  = return ()


//Start w = execIO main w

//////////////

//from gast import class gast
import MersenneTwister
import gen
import GenEq
import genLibTest
import testable
//import confSM
import stdProperty

derive bimap []

ggen{|MetaData|} _ = [zero]
//ggen{|Id|} _ = ["a"]
//derive ggen
//	Token, TokenType, Position, BraceType, BraceStyle
//derive gEq
//	BasicType
derive genShow
	Token, TokenType, Position, BraceType, BraceStyle
genShow{|Error|} _ _ e rest = [toString e: rest] 

import StdArray

(subStringOf) infix :: String String -> Bool
(subStringOf) sub string =
	if	(string == "")
		(sub == "")
		if	(sub == string % (0,size sub - 1) )
			True
			(sub subStringOf (string % (1,size string - 1) ) )

prop_scanner :: String -> Property
prop_scanner input =
	(not (isEmpty errors)) ==> (
			("&" subStringOf input) || ("|" subStringOf input)
	)
where (tokens, errors) = scanner input

Start = ttestn 1000 prop_scanner
////////////////////