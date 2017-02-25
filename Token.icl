implementation module Token

from Misc import :: Position
import GenString
import CustomStdEnv

scanner :: String -> [Token]
scanner _ = []

derive gString Token, TokenType, BraceType, BraceStyle

instance toString Token			where toString x = gString{|*|} x
instance toString TokenType		where toString x = gString{|*|} x
instance toString BraceType		where toString x = gString{|*|} x
instance toString BraceStyle	where toString x = gString{|*|} x


Start = 0