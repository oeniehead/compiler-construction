implementation module MainBinding

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
import BindingAnalysis

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
	case compile string of
		Left msg		 = withWorld \w.
			((), snd (fclose (stderr <<< msg) w) )
		Right (ast, log) =
			print ("AST:\n" +++ (prettyPrint ast) +++ (errorsToString log))
where
	compile :: String -> Either String (AST, [Error])
	compile prog = uptoBinding
				prog
				(const Nothing)
				(\pErrors -> errorsToString pErrors)
				(const Nothing)
				(\bErrors -> errorsToString bErrors)


Start w = execIO main w