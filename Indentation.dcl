definition module Indentation

/*
	Pretty printer combinator library
*/

import CustomStdEnv
import StdOverloaded

:: Show

prettyPrint :: Show -> String

rtrn		:: String -> Show
indent		:: 			 Show //increase the internal indentation by one
unindent	:: 			 Show //decrease the internal indentation by one
nl			::			 Show //add a newline, start the newline with the proper indentation
instance + Show			  //concat args

indentnl :== indent + nl
unindentnl :== unindent + nl
// indentBlock :: Show -> Show
indentBlock block :== indentnl + block + unindentnl

