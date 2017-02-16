implementation module Misc

import StdMisc
from StdOverloaded import class zero
import CustomStdEnv

nextPos :: Position Char -> Position
nextPos {line, col} c = case c of
	'\n'	= {line = line + 1, col = 0      }
	_		= {line = line    , col = col + 1}

instance zero Position where zero = {line = 0, col = 0}

toBeImplemented :: .a
toBeImplemented = abort "Program evaluated unimplemented function"