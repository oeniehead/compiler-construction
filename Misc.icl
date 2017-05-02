implementation module Misc

import StdMisc
from StdOverloaded import class zero
import CustomStdEnv
import GenString
import GenEq
import Data.List

nextPos :: Position Char -> Position
nextPos {line, col} c = case c of
	'\n'	= {line = line + 1, col = 0      }
	_		= {line = line    , col = col + 1}

derive gString Position
derive gEq Position

instance toString Position	where toString p	= gString{|*|} p 
instance ==	Position		where == a b		= gEq{|*|} a b
instance zero Position		where zero			= {line = 0, col = 0}

tbi :: .a
tbi = abort "Program evaluated unimplemented function"

concat :: [String] -> String
concat l = foldr (+++) "" l

(separatedBy) infixl :: [a] a -> [a]
(separatedBy) l sep = case l of
	[] = []
	[a] = [a]
	[a:as] = [a, sep : (as separatedBy sep)]

delimit :: [String] String -> String
delimit strings del = concat (strings separatedBy del)

printAllnl :: [a] -> String | toString a
printAllnl list = concat (map (\a.toString a +++ "\n") list)