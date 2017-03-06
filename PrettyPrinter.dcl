definition module PrettyPrinter

import SPLParser
import Indentation

prettyPrint :: a -> String | toShow a

class toShow a 	:: a 	-> Show

instance toShow AST