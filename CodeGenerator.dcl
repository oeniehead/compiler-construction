definition module CodeGenerator

import Error
import Data.Either

:: CGInst
:: CGArg

uptoCodeGeneration ::
	String
	([Error] -> Maybe a)
	([Error] -> a)
	([Error] -> Maybe a)
	([Error] -> a)
	([Error] -> a)
	([Error] -> a)
		-> Either a (String, [Error])
		
instance toString CGArg
instance toString CGInst