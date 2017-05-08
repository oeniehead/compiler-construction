definition module Misc

from StdOverloaded import class zero
import StdMisc
import GenString, GenEq

:: Position = { line :: Int, col :: Int }

derive gString Position
derive gEq Position

instance toString	Position
instance ==			Position
instance zero		Position


nextPos :: Position Char -> Position

toBeImplemented :== tbi
tbi :: .a

(separatedBy)	infixl		:: [a] a			-> [a] // [1,2,3] separatedBy 0 = [1,0,2,0,3]

(<++)			infixl 9	:: String a		-> String | toString a
concat						:: [a] 			-> String | toString a
delimit						:: [a] String	-> String | toString a
(delimitBy)		infixl		:: [a] String	-> String | toString a
// delimit ["a", "b"] ", " = "a, b"


printAllnl :: [a] -> String | toString a