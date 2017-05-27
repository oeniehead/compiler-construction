implementation module MainTyping

import System.CommandLine
import System.IO
import Control.Monad
import Control.Applicative
import Data.Functor
import Data.List
import StdGeneric

import CustomStdEnv

import Scanner

import Error
import Misc
import PrettyPrinter
import Data.Either
import TypeChecker

getCl :: IO [String]
getCl = IO getCommandLine

getArgs :: IO [String]
getArgs = fmap (drop 1) getCl

main :: IO ()
main =
	getArgs										>>= \args.
	let file = hd args in
	//print ("File: " +++ file)					>>|
	readFileM file								>>= \string.
	//print ("contents: \"" +++ string +++ "\"")	>>|
	case compile string of
		Left msg		 = withWorld \w.
			((), snd (fclose (stderr <<< msg) w) )
		Right (ast, log) =
			print ((prettyPrint ast) +++ (errorsToString log))
where
	compile :: String -> Either String (AST, [Error])
	compile prog = uptoTypeInference
				prog
				(const Nothing)
				(\pErrors -> "Scan/parse errors:\n"	+++ (errorsToString pErrors))
				(\bErrors -> "Binding errors:\n"	+++ (errorsToString bErrors))
				(\tErrors -> "Typing errors:\n"		+++ (errorsToString tErrors))


Start w = execIO main w