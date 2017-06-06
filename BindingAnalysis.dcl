definition module BindingAnalysis

import AST
import Error
from Data.Either import :: Either

import StdGeneric
import GenString

/*class doOrder a :: OrderGraph a -> OrderGraph
class buildGraphFrom a :: OrderItem [String] a -> OrderGraph

:: OrderItem
	= VarItem String Position			// Variable name
	// Function name with amount of arguments and use of return type
	// Uses maybe to indicate possible return confusion
	| FuncItem String Position
	| CallsItem String Int Bool Position

// Function name, arg amounts, returns value
:: Signature
	= VarSig String
	| FuncSig String Int Bool

:: OrderGraph :== ([(OrderItem, OrderItem)], [Signature], [Error])*/

doBindingAnalysis :: AST -> Either [Error] (AST, [Error])

uptoBinding ::
	String
	([Error] -> Maybe a)
	([Error] -> a)
	([Error] -> Maybe a)
	([Error] -> a)
		-> Either a (AST, [Error])