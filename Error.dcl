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
	| TypeChecker
	// etc.

derive gString Stage, Severity

instance toString Error, Stage, Severity
