definition module GenString

import StdGeneric

// Based on GenEq.icl

generic gString a  :: a -> String

// base cases
derive gString Int, Char, Bool, Real, String, UNIT, PAIR, EITHER, CONS of c, RECORD, FIELD of c, OBJECT

// standard types
derive gString [], (,), (,,), (,,,)