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

(separatedBy) infixl :: [a] a -> [a]
(separatedBy) l sep = case l of
	[] = []
	[a] = [a]
	[a:as] = [a, sep : (as separatedBy sep)]

(<++) infixl 9 :: String a -> String | toString a
(<++) str x = str +++ toString x

concat :: [a] -> String | toString a
concat l = foldl (<++) "" l

delimit :: [a] String -> String | toString a
delimit as del = concat ((map toString as) separatedBy del)

(delimitBy) infixl :: [a] String -> String | toString a
(delimitBy) l s = delimit l s


printAllnl :: [a] -> String | toString a
printAllnl list = concat (map (\a.toString a +++ "\n") list)