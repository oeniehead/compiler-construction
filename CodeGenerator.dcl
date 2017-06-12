definition module CodeGenerator

import Error
import Data.Either

import AST

codeGenerator :: AST -> (Maybe String, [Error])

uptoCodeGeneration ::
	String
	([Error] -> Maybe a)
	([Error] -> a)
	([Error] -> Maybe a)
	([Error] -> a)
	([Error] -> a)
	([Error] -> a)
		-> Either a (String, a)
