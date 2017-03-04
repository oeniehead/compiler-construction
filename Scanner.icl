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
					'}'	= return (Token (Brace Close Curly) ("}") pos)
					'('	= return (Token (Brace Open Round) ("(") pos)
					')'	= return (Token (Brace Close Round) (")") pos)
					'['	= return (Token (Brace Open Square) ("[") pos)
					']'	= return (Token (Brace Open Square) ("]") pos)
					
					/** Other single character tokens **/
					';' = return (Token TerminatorToken ";" pos)
					'.' = return (Token Dot "." pos)
					',' = return (Token Comma "," pos)
					'\'' = return (Token SingleQuote "'" pos) // todo: parsing of character constants 'c'?
					
					/** Splittable binary operators **/
					'+' = return (Token Operator "+" pos)
					'*' = return (Token Operator "*" pos)
					'%' = return (Token Operator "%" pos)
					
					/** Complex cases **/
					':' = branch1 pos
					'-' = branch2 pos
					'>' = branch3 pos
					'<' = branch4 pos
					'&' = branch5 pos
					'|' = branch6 pos
					'/' = branchFwdSlash pos
					'=' = branchEq pos
					'!' = branchExcl pos
					
					/** Strings and integers **/
					_ 	= 	if (isDigit c) (readInteger (toString c) pos) (
							if (isAlpha c) (readString  (toString c) pos) (
							
							/** Skip space **/
							if (isSpace c) (readToken) (
										/** We have found garbage **/
										(log pos ERROR ("Illegal character '" +++ (toString c) +++ "'")) >>|
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
					
/** Difference between minus token and type arrow**/
branch2 :: Position -> Scanner Token
branch2 pos = peek >>= \next.
				case next of
					Just '>' 	= read >>| return (Token TypeArrow "->" pos)
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
					_ 			= log pos ERROR "Illegal token '&'" >>| return (Token Unscannable "&" pos)
					
/** Reading || **/
branch6 :: Position -> Scanner Token
branch6 pos = peek >>= \next.
				case next of
					Just '|' 	= read >>| return (Token Operator "||" pos)
					_ 			= log pos ERROR "Illegal token '|'" >>| return (Token Unscannable "|" pos)

/** Difference between / and the two comment types */
branchFwdSlash :: Position -> Scanner Token
branchFwdSlash pos = peek >>= \next.
				case next of
					Just '/' 	= read >>| skipLine >>| readToken
					Just '*'	= read >>| endMultiLineComment >>| readToken
					_ 			= return (Token Operator "/" pos)
where
	skipLine :: Scanner ()
	skipLine = read >>= \c.
		case c of
			Just '\n'	= return ()
			Nothing		= return ()
			_			= skipLine
	endMultiLineComment :: Scanner ()
	endMultiLineComment = read >>= \c.
		case c of
			Just '*'	= readedStar
			Nothing		= logHere WARN "Last multiline comment has no *\\ terminator"
			_			= endMultiLineComment
	where
		readedStar = read >>= \c. //spelling mistake is on purpose, readStar could be understood has "having to read a star" i.s.o. "just read a star"
			case c of
				Just '/'	= return ()
				Just '*'	= readedStar
				Nothing		= logHere WARN "Last multiline comment has no *\\ terminator"

branchEq :: Position -> Scanner Token
branchEq pos = peek >>= \c.
	case c of
		Just '='	= read >>| return (Token Operator "==" pos)
		_			= return (Token Assignment "=" pos)

branchExcl :: Position -> Scanner Token
branchExcl pos = peek >>= \c.
	case c of
		Just '='	= read >>| return (Token Operator "!=" pos)
		_			= return (Token Operator "!" pos)

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
					Just n 		= if(isAlphanum n || n == '_') (read >>| readString (p +++ (toString n)) pos) (return (Token StringToken p pos))
					_ 			= return (Token StringToken p pos)

//Start = runScanner "" scan
scan = readString "c" zero//Token StringToken c

//Start = runScanner "|" scan1
scan1 = readToken//should be Unscannable

//Start = runScanner "c" readToken

//Start = runScanner "c" read

//Start = runScanner "" read
		
//Start = scanner "Fred123.hd.tl.snd.snd.fst"

//Start = scanner string1
string1 = ("aaa //comment \n"
	   +++ "bbb /*mulitiline \n"
	   +++ "comment*/ ccc /* falling off")
