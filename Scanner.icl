implementation module Scanner

import Token
import StringScanner
import Misc

scanner :: String -> [Token]
scanner input = tokens
	where
		(tokens, errors) = runScanner input readTokens
		
readTokens :: [Token] -> Scanner [Token]
readTokens prev = readToken >>= \token.
					peek >>> \next.
						case next of
							Nothing = return (prev ++ token)
							_		= readTokens (prev ++ token)
				

readToken :: Scanner Token
readToken =
	read 	>>= \char.
	getPos 	>>= \pos.
		case char of
			/** We have hit the end **/
			Nothing	= logHere FATAL "readToken called while buffer was empty"
			Just c =
				case c of
					/** All bracket types **/
					'{'	= return (Token (Brace Open Curly) ("{") pos)
					'}'	= return (Token (Brace Open Curly) ("}") pos)
					'('	= return (Token (Brace Open Round) ("(") pos)
					')'	= return (Token (Brace Close Round) (")") pos)
					'['	= return (Token (Brace Open Square) ("[") pos)
					']'	= return (Token (Brace Open Square) ("]") pos)
					
					/** Other single character tokens **/
					';' = return (Token TerminatorToken ";" pos)
					'.' = return (Token Dot "." pos)
					',' = return (Token Comma "," pos)
					
					/** Splittable binary operators **/
					'+' = return (Token Operator "+" pos)
					'*' = return (Token Operator "*" pos)
					'/' = return (Token Operator "/" pos)
					'%' = return (Token Operator "%" pos)
					
					/** Complex cases **/
					':' = branch1
					'-' = branch2
					'>' = branch3
					'<' = branch4
					'&' = branch5
					'|' = branch6
					
					/** Strings and integers **/
					_ 	= 	if (isDigit c) (readInteger (toString c) pos) (
							if (isAlpha c) (readString  (toString c) pos) (
							
							/** Skip space **/
							if (isSpace c) (readToken) (
										/** We have found garbage **/
										(return (Token Unscannable (toString c) pos))
							)))	
					

/** Difference between type indicators and array concatenation **/					 
branch1 :: Position -> Scanner Token
branch1 pos = peek >>= \next.
				case next of
					Just ':' = read >>= return (Token TypeIndicator "::" pos)
					_ = return (Token Operator ":" pos)
					
/** Difference between minus operator and type arrow **/
branch2 :: Position -> Scanner Token
branch2 pos = peek >>= \next.
				case next of
					Just '>' 	= read >>= return (Token TypeArrow "::" pos)
					_ 			=  return (Token Operator "-" pos)
					
/** Difference between > and >= **/
branch3 :: Position -> Scanner Token
branch3 pos = peek >>= \next.
				case next of
					Just '=' 	= read >>= return (Token Operator ">=" pos)
					_ 			=  return (Token Operator ">" pos)
					
/** Difference between < and <= **/
branch4 :: Position -> Scanner Token
branch4 pos = peek >>= \next.
				case next of
					Just '=' 	= read >>= return (Token Operator "<=" pos)
					_ 			=  return (Token Operator "<" pos)
					
/** Reading && **/
branch5 :: Char TokenType Position -> Scanner Token
branch5 pos = peek >>= \next.
				case next of
					Just '&' 	= read >>= return (Token Operator "&&" pos)
					_ 			=  return (Token Unscannable "&" pos)
					
/** Reading || **/
branch6 :: Char TokenType Position -> Scanner Token
branch6 pos = peek >>= \next.
				case next of
					Just '|' 	= read >>= return (Token Operator "||" pos)
					_ 			=  return (Token Unscannable "|" pos)
				
/** Read a string of numerical chars **/
readInteger :: String Position -> Scanner Token
readInteger p pos = peek >>= \next.
				case next of
					Just n 		= if(isDigit x) (read >>= readInteger (p +++ (toString n)) pos) (return (Token NumToken p pos))
					_		 	= return (Token NumToken p pos)
					
/** Read a string of alphanumerical chars, is only called when a character is found **/
readString :: String Position -> Scanner Token
readString p pos = peek >>= \next.
				case next of
					Just n 		= if(isAlphanum x) (read >>= readString (p +++ (toString n)) pos) (return (Token StringToken p pos))
					_ 			= return (Token StringToken p pos)

Start = readToken