implementation module Error

import GenString
import Misc
import CustomStdEnv

makeError :: Position Severity Stage String -> Error
makeError p sev stage msg = {pos=p, severity=sev, stage=stage, message=msg}

derive gString Stage, Severity

instance toString Error		where
	toString {pos={line,col}, severity, stage, message}
	= concat [ toString severity
			 , case severity of //extra space for lining everything up
			 	WARN	= " "
			 	INFO	= " "
			 	_		= "" 
			 , "[" +++ (toString line) +++ "," +++ (toString col) +++ "] "
			 , "(" +++ toString stage +++ "): "
			 , message
			 ]
instance toString Stage		where toString s = gString{|*|} s
instance toString Severity	where toString s = gString{|*|} s