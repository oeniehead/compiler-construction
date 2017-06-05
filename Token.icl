implementation module Token

from Misc import :: Position
import GenString
import CustomStdEnv

getOriginal :: Token -> String
getOriginal (Token _ s _) = s

derive gString Token, TokenType, BraceType, BraceStyle
derive gEq Token, TokenType, BraceType, BraceStyle

instance toString Token			where toString x = gString{|*|} x
instance toString TokenType		where toString x = gString{|*|} x
instance toString BraceType		where toString x = gString{|*|} x
instance toString BraceStyle	where toString x = gString{|*|} x

instance == TokenType			where == a b = gEq{|*|} a b
instance == BraceStyle			where == a b = gEq{|*|} a b
instance == BraceType			where == a b = gEq{|*|} a b
instance == Token				where == a b = gEq{|*|} a b

//Start = 0