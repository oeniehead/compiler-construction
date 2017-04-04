module test

import TestTooling
import CustomStdEnv
import StdDebug

// -- Indentation
import qualified Indentation

indent_prop :: String -> Bool
indent_prop s = 'Indentation'.show2string ('Indentation'.rtrn s) == s

// -- Parsing and printing
import SPLParser
import PrettyPrinter
import Scanner
import Data.Either

derive gEq	Decl, VarDecl, FunDecl, Stmt, Expr, 
			FunCall, Type, BasicType, IdWithFields, Field, BinOp, UnOp, Id
gEq{|MetaData|} _ _ = True

instance == Decl where (==) a b = a === b

// print the ast and parse it again
prop_printparse :: AST -> Property
prop_printparse ast
# astString = prettyPrint ast
# (tokens, errors) = scanner astString
= case errors of
	[_:_]	= trace_n (printErrors "\nerrors in scanner:" errors) (prop False)
	_		= case parser tokens of
		Left es		= trace_n (printErrors "\nerrors in parser:" es) (prop False)
		Right ast`	= ast =.= ast`

printErrors :: String [Error] -> String
printErrors s es = concat ([s:map toString es] separatedBy "\n")

// -- Start
Start =
	[
		["Indentation: show (rtrn s) == s"],
		test indent_prop,
		
		["SPLParserTests"],
		SPLParserTests,
		
		["prop_printparse"],
		test prop_printparse //functies moeten gegenereerd worden met minstens 1 statement
		
		
	] separatedBy ["\n"]

