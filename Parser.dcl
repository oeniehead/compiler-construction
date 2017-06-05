definition module Parser

import Token
import AST
import Error
from Data.Either import :: Either

/* Parse an array of scanned tokens
 * [Token]	: the tokens to be parsed
 * Returns	:
 *		- (Just (ok, ast), log): if it is possible to parse a syntactically correct part of the tokens.
 								 ok indicates if the whole input was syntactically correct
 		- (Nothing       , log): if it was not possible to generate any parse results
 */
parser :: [Token] -> (Maybe (Bool, AST), [Error])

/* Scan and parse
 * String				: the program
 * [Error] -> Maybe a   : should we stop and return (Left a) if there are scanner errors or continue(Nothing)?
 * [Error] -> a			: what should we do if the parser failed?(you get the errors of both scanner and parser)
 * [Error] -> Maybe a	: If the parser can ignore a syntactically incorrect part of the code, should we continue?
 * Returns				: (Left a) with the specified a if failed
 *						  (Right (ast,log)) if succeeded, with the log of the scanner
 */
uptoParse :: String ([Error] -> Maybe a) ([Error] -> a) ([Error] -> Maybe a) -> Either a (AST,[Error])
