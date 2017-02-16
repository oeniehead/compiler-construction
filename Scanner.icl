implementation module Scanner

import Token
import StringScanner
import Misc

scanner :: String -> [Token]
scanner _ = toBeImplemented

readToken :: Scanner Token
readToken = toBeImplemented
/*
readToken
	= read >>= \char.
		case char of
			...		=				return (StringToken ...)
			...		= read ....	>>= return (StringToken ...)
		...	return (StringToken ...)
*/

Start = readToken