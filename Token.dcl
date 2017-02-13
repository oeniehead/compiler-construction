definition module Token



:: Token = Token TokenType String Position

:: TokenType
	= Assignment
	| StringToken
	| NumToken
	| TerminatorToken
	| Brace BraceType BraceStyle
	| UnaryOperator
	| BinaryOperator
	| Dot
	| Comma
	| TypeIndicator
	| TypeArrow

:: BraceType	= Open  | Close
:: BraceStyle	= Curly | Round | Square

:: Position = { line :: Int, col :: Int }


