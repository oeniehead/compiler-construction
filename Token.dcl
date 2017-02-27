definition module Token

from Misc import :: Position
import GenString
import CustomStdEnv

:: Token = Token TokenType String Position

:: TokenType
	= Assignment
	| StringToken // Starts with an alphabetic char, but can be alphanumerical or '_' from there on
	| NumToken
	| TerminatorToken
	| Brace BraceType BraceStyle
	| Operator  // je kunt unaire en binaire operatoren niet scheiden tijdens het parsen
	| Dot
	| Comma
	| SingleQuote
	| TypeIndicator
	| TypeArrow
	| EOFToken
	| Unscannable // if the scanning has failed, but the scanner is able to recover

:: BraceType	= Open  | Close
:: BraceStyle	= Curly | Round | Square

derive gString Token, TokenType, BraceType, BraceStyle

instance toString Token
instance toString TokenType
instance toString BraceType
instance toString BraceStyle

instance == TokenType
instance == BraceType
instance == BraceStyle
