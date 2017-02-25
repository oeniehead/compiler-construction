definition module Misc

from StdOverloaded import class zero
import StdMisc
import GenString

:: Position = { line :: Int, col :: Int }

instance zero Position
instance toString Position

derive gString Position

nextPos :: Position Char -> Position

toBeImplemented :: .a

concat :: [String] -> String
(separatedBy) infixl :: [a] a -> [a] // [1,2,3] separatedBy 0 = [1,0,2,0,3]
delimit :: [String] String -> String // delimit ["a", "b"] ", " = "a, b"
