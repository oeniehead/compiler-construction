implementation module Scanner

import Token
import StringScanner
import Misc
import CustomStdEnv


scanner :: String -> ([Token], [Error])
scanner input = runScanner input (readTokens [])
		
readTokens :: [Token] -> Scanner [Token]
readTokens prev = readToken >>= \token.
						case token of
							(Token EOFToken _ _)	= return (prev ++ [token])
							_						= readTokens (prev ++ [token])
				

readToken :: Scanner Token
readToken =
	read 	>>= \char.
	getPos 	>>= \pos.
		case char of
			/** We have hit the end **/
			Nothing	= return (Token EOFToken "" pos)
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
					':' = branch1 pos
					'-' = branch2 pos
					'>' = branch3 pos
					'<' = branch4 pos
					'&' = branch5 pos
					'|' = branch6 pos
					
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
branch1 pos = peek >>= qwe
where
	qwe :: ((Maybe Char) -> Scanner Token)
	qwe = \next.
				case next of
					Just ':' = read >>| return (Token TypeIndicator "::" pos)
					_ = return (Token Operator ":" pos)
					
/** Difference between minus operator and type arrow **/
branch2 :: Position -> Scanner Token
branch2 pos = peek >>= \next.
				case next of
					Just '>' 	= read >>| return (Token TypeArrow "::" pos)
					_ 			=  return (Token Operator "-" pos)
					
/** Difference between > and >= **/
branch3 :: Position -> Scanner Token
branch3 pos = peek >>= \next.
				case next of
					Just '=' 	= read >>| return (Token Operator ">=" pos)
					_ 			=  return (Token Operator ">" pos)
					
/** Difference between < and <= **/
branch4 :: Position -> Scanner Token
branch4 pos = peek >>= \next.
				case next of
					Just '=' 	= read >>| return (Token Operator "<=" pos)
					_ 			=  return (Token Operator "<" pos)
					
/** Reading && **/
branch5 :: Position -> Scanner Token
branch5 pos = peek >>= \next.
				case next of
					Just '&' 	= read >>| return (Token Operator "&&" pos)
					_ 			=  return (Token Unscannable "&" pos)
					
/** Reading || **/
branch6 :: Position -> Scanner Token
branch6 pos = peek >>= \next.
				case next of
					Just '|' 	= read >>| return (Token Operator "||" pos)
					_ 			=  return (Token Unscannable "|" pos)
				
/** Read a string of numerical chars **/
readInteger :: String Position -> Scanner Token
readInteger p pos = peek >>= \next.
				case next of
					Just n 		= if(isDigit n) (read >>| readInteger (p +++ (toString n)) pos) (return (Token NumToken p pos))
					_		 	= return (Token NumToken p pos)
					
/** Read a string of alphanumerical chars, is only called when a character is found **/
readString :: String Position -> Scanner Token
readString p pos = peek >>= \next.
				case next of
					Just n 		= if(isAlphanum n) (read >>| readString (p +++ (toString n)) pos) (return (Token StringToken p pos))
					_ 			= return (Token StringToken p pos)

Start = readToken