definition module Error

from Misc import :: Position

import Misc

:: Error = Error Position Severity String

:: Severity
	= FATAL
	| ERROR
	| WARN
	| INFO
	| DEBUG
	| TRACE