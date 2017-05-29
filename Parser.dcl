definition module Parser

import Token
import AST
import Error
from Data.Either import :: Either

parser :: [Token] -> Either [Error] AST

/* Scan and parse
 * String				: the program
 * [Error] -> Maybe a   : should we stop and return (Left a) if there are scanner errors?
 * [Error] -> a			: what should we do if the parser failed?(you get the errors of both scanner and parser)
 * Return				: (Left a) with the specified a if failed
 *						  (Right (ast,log)) if succeeded, with the log of the scanner
 */
uptoParse :: String ([Error] -> Maybe a) ([Error] -> a) -> Either a (AST,[Error])

