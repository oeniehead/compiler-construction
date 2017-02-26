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
	getPos 	>>= \pos.
	read 	>>= \char.
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
					'\'' = return (Token SingleQuote "'" pos)
					
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

//Start = runScanner "" scan
scan = readString "c" zero//Token StringToken c

//Start = runScanner "|" scan1
scan1 = readToken//Token Op ||, should be Unscannable

//Start = runScanner "" scan2
scan2 = branch6 zero//Token Unscannable

//Start = runScanner "e" scan3
scan3 = branch6 zero//Token Unscannable

//Start = runScanner "|" scan4
scan4 = branch6 zero//Token Op ||

//Start = runScanner "d" (readTokens [])
/*readTokens prev = readToken >>= \token.
						case token of
							(Token EOFToken _ _)	= return (prev ++ [token])
							_						= readTokens (prev ++ [token])*/
//Start = runScanner "c" readToken//heap full

//Start = runScanner "c" read//Just c

//Start = runScanner "" read//Nothing

//Start = runScanner "c" scan5//heap full
scan5 =
	getPos 	>>= \pos.
	read 	>>= \char.
		case char of
			Just c =
				if (isAlpha c) (readString  (toString c) pos)
				(return undef)

//Start = runScanner "c" scan6//(Just c, (0,0))
scan6 =
	getPos 	>>= \pos.
	read 	>>= \char.
		return (char,pos)

//Start = runScanner "c" scan7//heap full
scan7 =
	read 	>>= \(Just c).
				if (isAlpha 'c') (readString  "c" zero)
				(return undef)

//Start = runScanner "c" scan8//heap full
scan8 =
	read 	>>| (readString  "c" zero)

//Start = runScanner "c" scan9//Just c
scan9 =
	read 	>>| read

//Start = runScanner "c" scan10//heap full
scan10 = (readString  "c" zero)

//Start = runScanner "c" scan11//heap full
scan11 = peek >>= \next.
				case next of
					Just n 		= if(isAlphanum n) (read >>| readString ("c" +++ (toString n)) zero) (return (Token StringToken "c" zero))
					_ 			= return (Token StringToken "c" zero)

//Start = runScanner "c" scan12//heap full
scan12 = case (Just 'c') of
			Just 'c' 		= if(isAlphanum 'c') (read >>| readString "c" zero) (return (Token StringToken "c" zero))
			_ 			= return (Token StringToken "c" zero)

//Start = runScanner "c" scan13//heap full, looks like scan8. Maybe the recursive definition of readString is the problem
scan13 = if(isAlphanum 'c') (read >>| readString "c" zero) (return (Token StringToken "c" zero))

//Start = runScanner "abc" scan14//abc
scan14 = read >>= \a.
		read >>= \b.
		read >>= \c.
		return [a,b,c]

//Start = runScanner "abc " scan15//heap full. Apperently recursive monadic definitions are problematic.
scan15 = read >>= \(Just c). if (c == 'c')
			(read >>| scan15)
			(return "finish") /*** Edit: vandaag geeft ie _wel_ finish als output, gisteren niet!! ***/

// weet niet of het porbleem is dat ie de recursie wilt uitschrijven of dat er daadwerkelijk geen normaalvorm is.
// Possible solution 1: apply tail recursion, poging om uitschrijven te voorkomen
//Start = runScanner "abc " scan16//finish
scan16 = read >>= \(Just c). if (not (c == 'c'))
			(return "finish")
			(read >>| scan15)
