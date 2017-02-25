implementation module Misc

import StdMisc
from StdOverloaded import class zero
import CustomStdEnv
import GenString

nextPos :: Position Char -> Position
nextPos {line, col} c = case c of
	'\n'	= {line = line + 1, col = 0      }
	_		= {line = line    , col = col + 1}

derive gString Position

instance zero Position		where zero			= {line = 0, col = 0}
instance toString Position	where toString p	= gString{|*|} p 

toBeImplemented :: .a
toBeImplemented = abort "Program evaluated unimplemented function"

concat :: [String] -> String
concat l = foldr (+++) "" l

(separatedBy) infixl :: [a] a -> [a]
(separatedBy) l sep = case l of
	[] = []
	[a] = [a]
	[a:as] = [a, sep : (as separatedBy sep)]

delimit :: [String] String -> String
delimit strings del = concat (strings separatedBy del)