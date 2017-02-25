implementation module Error

import GenString
import Misc

derive gString Severity, Error

instance toString Error		where toString e = gString{|*|} e 
instance toString Severity	where toString s = gString{|*|} s