definition module TypeChecker

import Spec
import Parser
from Data.Maybe import :: Maybe
from Error import :: Error

typeInference :: AST -> ((Maybe AST), [Error])

/* Scan, parse, binding analysis and typeinference
 * String				: the program
 * [Error] -> Maybe a   : should we stop and return (Left a) if there are scanner errors?
 * [Error] -> a			: what should we do if the parser failed?(you get the errors of both scanner and parser)
 * [Error] -> a			:  what should we do if the type inference failed?(you get all the previous errors)
 * Return				: (Left a) with the specified a if failed
 *						  (Right (ast,log)) if succeeded, with the log of the scanner
 */
uptoTypeInference ::
	String
	([Error] -> Maybe a)	//Scanner
	([Error] -> a)			//Parser fail
	([Error] -> Maybe a)	//Parser recover
	([Error] -> a)			//Binding fail
	([Error] -> a)			//TypeChecker fail
		-> Either a (AST, [Error])


typeCheckerTests :: [Testcase]