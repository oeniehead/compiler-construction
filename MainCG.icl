implementation module MainCG

import System.CommandLine
import System.IO
import Control.Monad
import Control.Applicative
import Data.Functor
import Data.List
import StdGeneric

import CustomStdEnv

import Scanner
import SPLParser
import Error
import Misc
import PrettyPrinter
import Data.Either
import TypeChecker
import CodeGenerator

getCl :: IO [String]
getCl = IO getCommandLine

getArgs :: IO [String]
getArgs = fmap (drop 1) getCl

main :: IO ()
main =
	//print "=========================="			>>|
	getArgs										>>= \args.
	let file = hd args in
	//print ("File: " +++ file)					>>|
	readFileM file								>>= \string.
	//print ("contents: \"" +++ string +++ "\"")	>>|
	case compile string of
		Left msg		 = print msg
		Right (instructions, log) =
			print instructions
where
	compile :: String -> Either String ([CGInst], [Error])
	compile prog = uptoCodeGeneration
				prog
				(const Nothing)
				(\pErrors -> errorsToString pErrors)
				(\bErrors -> errorsToString bErrors)
				(\tErrors -> errorsToString tErrors)
				(\cErrors -> errorsToString cErrors)


Start w = execIO main w