definition module StringScanner

import Control.Monad
import Token
import Error

:: Scanner a

instance Functor		Scanner
instance Applicative	Scanner
instance Monad			Scanner

hasNext :: Scanner Bool				// Are there characters left?
read	:: Scanner (Maybe Char)		// Read a character
peek	:: Scanner (Maybe Char)		// Read a character without consuming it
getPos  :: Scanner Position			// Get the current position

//produce	:: Token	-> Scanner ()		// Produce a token //not needed
log		:: Error			-> Scanner ()	// Log an error
logHere :: Severity String	-> Scanner ()	// Log at the current position

runScanner :: String (Scanner a) -> (a, [Error])