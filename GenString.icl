implementation module GenString

import StdGeneric
import CustomStdEnv
import StdList

generic gString a  :: a -> String
gString{|Int|} 	x						= toString x
gString{|Char|} x						= toString x
gString{|Bool|} x						= toString x
gString{|Real|} x						= toString x
gString{|String|} x						= "\"" +++ x +++ "\""
gString{|UNIT|}			UNIT				= ""
gString{|PAIR|}			fx fy (PAIR x y)	= (fx x) +++ " " +++ (fy y)
gString{|EITHER|}		fl fr (LEFT l) 		= fl l
gString{|EITHER|}		fl fr (RIGHT r) 	= fr r
gString{|CONS of c|}	f     (CONS x) 		= if (c.gcd_arity == 0)
												c.gcd_name
												("(" +++ c.gcd_name +++ " " +++ (f x) +++ ")")
gString{|RECORD|}		f     (RECORD x)  	= "{" +++ (f x) +++ "}"
gString{|FIELD of c|}	f     (FIELD x)		= (if (c.gfd_index == 0) "" ",") // start with comma if this is not the first field
												+++ c.gfd_name +++ "=" +++ (f x)
gString{|OBJECT|}		f     (OBJECT x)	 = f x
gString{|(,)|}   fx fy    (x,y)			= "(" +++ (fx x) +++ "," +++ (fy y) +++ ")"
gString{|(,,)|}  fx fy fz (x,y,z)		= "(" +++ (fx x) +++ "," +++ (fy y) +++ "," +++ (fz z) +++ ")"
gString{|(,,,)|} fx fy fz fu (x,y,z,u)	= "(" +++ (fx x) +++ "," +++ (fy y) +++ "," +++ (fz z) +++ "," +++ (fu u) +++ ")"
gString{|[]|}  f l				= "[" +++ (delimit (map f l) ",") +++ "]"

derive gString Maybe

concat :: [String] -> String
concat l = foldr (+++) "" l

(separatedBy) infixl :: [a] a -> [a]
(separatedBy) l sep = case l of
	[] = []
	[a] = [a]
	[a:as] = [a, sep : (as separatedBy sep)]

delimit :: [String] String -> String
delimit strings del = concat (strings separatedBy del)