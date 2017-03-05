implementation module Indentation

/*
	Pretty printer combinator library
*/

import CustomStdEnv
import StdOverloaded
import Misc

:: SHOW =	{ print  :: [String]
			, indent :: Int      }
:: Show = Show (SHOW -> SHOW)
sh0 =	{ print  = []
		, indent = 0 }

prettyPrint :: Show -> String
prettyPrint (Show sh) = foldl (+++) "" (reverse (sh sh0).print)

rtrn :: String -> Show
rtrn s = Show \sh -> {sh & print = [s:sh.print]}

instance +++ Show where
	(+++) (Show f) (Show g) = Show (g o f)

nl :: Show
nl = Show \sh -> { sh & print = [toString ['\n':repeatn sh.indent '\t']:sh.print]}

indent :: Show
indent = Show \sh -> {sh & indent = inc sh.indent}

unindent :: Show
unindent = Show \sh -> { sh & indent = dec sh.indent}


