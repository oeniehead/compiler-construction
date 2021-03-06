definition module Error

//from Misc import :: Position
import GenString
import Misc
import CustomStdEnv

:: Error =
	{ pos		:: Position
	, severity	:: Severity
	, stage		:: Stage
	, message	:: String
	}

makeError :: Position Severity Stage String -> Error

:: Severity
	= FATAL
	| ERROR
	| WARN
	| INFO
	| DEBUG
	| TRACE

:: Stage
	= Scanning
	| Parsing
	| Binding
	| TypeChecking
	| CodeGeneration 
	// etc.

derive gEq		Stage, Severity
derive gString	Stage, Severity

instance ==		Stage, Severity
instance toString Error, Stage, Severity

errorsToString errors :== printAllnl errors 