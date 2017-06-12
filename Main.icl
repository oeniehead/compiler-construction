implementation module Main

import System.CommandLine
import System.IO
import Control.Monad
import Data.Either
import StdArray
from Data.Func import $

import CustomStdEnv
import Error
import Misc
import PrettyPrinter
import Scanner, Parser, BindingAnalysis, TypeChecker, CodeGenerator

getCl :: IO [String]
getCl = IO getCommandLine

getArgs :: IO [String]
getArgs = fmap (drop 1) getCl

main :: IO ()
main =
	getArgs										>>= \args.
	if ( not (length args == 1))
		(print "Usage: spl.exe <source-file>")
		let file = hd args in
		readFileM file								>>= \string.
		case compile string of
			Left log		 = withWorld \w.
				((), snd (fclose (stderr <<< (errorsToString log)) w) )
			Right (ssmcode, log) =
				print (errorsToString log) >>|
				writeFileM ( (hd $ split file '.') +++ ".ssm" ) ssmcode
					
where
	compile :: String -> Either [Error] (String, [Error])
	compile prog 
	# (tokens, sLog) = scanner prog
	# (parseRes, pLog) = parser tokens
	| isNothing parseRes	= Left (sLog ++ pLog) 
	# (parseOk, ast) = fromJust parseRes
	# bindingRes = doBindingAnalysis ast
	= case bindingRes of
		Left bLog			= Left (sLog ++ pLog ++ bLog)
		Right (ast`,bLog)	=
			case typeInference ast` of
				(Nothing, tLog)		= Left (sLog ++ pLog ++ bLog ++ tLog)
				(Just ast``, tLog)	= case codeGenerator ast`` of
					(Nothing, cLog)		= Left (sLog ++ pLog ++ bLog ++ tLog ++ cLog)
					(Just ssmcode, cLog)=
						if parseOk
							(Right (ssmcode , sLog ++ pLog ++ bLog ++ tLog ++ cLog))
							(Left (sLog ++ pLog ++ bLog ++ tLog ++ cLog))

split :: String Char -> [String]
split s c = split` 0 s
where
	split` i s
	| i < size s
		| select s i == c	= [s % (0,i-1):split` 0 (s % (i+1, size s))]
		| otherwise			= split` (i+1) s
	| otherwise				= [s]


Start w = execIO main w