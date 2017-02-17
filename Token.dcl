definition module Token

from Misc import :: Position

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
	| TypeIndicator
	| TypeArrow
	| Unscannable // if the scanning has failed, but the scanner is able to recover

:: BraceType	= Open  | Close
:: BraceStyle	= Curly | Round | Square



