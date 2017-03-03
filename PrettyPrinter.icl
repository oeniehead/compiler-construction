implementation module PrettyPrinter

import Indentation
import SPLParser
import Misc

prettyPrint :: AST -> String
prettyPrint _ = toBeImplemented

curlyStyle :: String String Show -> Show
curlyStyle title arg body = rtrn (title +++ "(" +++ arg +++ ") {") +++ indent +++ nl
		+++ body +++ unindent +++ nl
		+++ rtrn "}"

func :: String String [String] Show -> Show
func type name args body
= rtrn (type +++ " ") +++ curlyStyle name (concat (args separatedBy ", ")) body
