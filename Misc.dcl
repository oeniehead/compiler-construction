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

toBeImplemented :: .a

concat :: [String] -> String
(separatedBy) infixl :: [a] a -> [a] // [1,2,3] separatedBy 0 = [1,0,2,0,3]
delimit :: [String] String -> String // delimit ["a", "b"] ", " = "a, b"
