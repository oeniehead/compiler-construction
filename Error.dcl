definition module Error

//from Misc import :: Position
import GenString
import Misc
import CustomStdEnv

:: Error = Error Position Severity String

:: Severity
	= FATAL
	| ERROR
	| WARN
	| INFO
	| DEBUG
	| TRACE

derive gString Severity, Error

instance toString Error
instance toString Severity