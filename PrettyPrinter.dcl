definition module PrettyPrinter

import SPLParser
from Indentation import :: Show

prettyPrint :: a -> String | toShow a

class toShow a 	:: a 	-> Show

instance toShow AST