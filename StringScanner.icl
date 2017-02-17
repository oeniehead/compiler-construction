implementation module StringScanner

from StdOverloaded import class zero
import CustomStdEnv
import StdMaybe
import Control.Monad
import Token
import Error
import Misc

:: Scanner a = Scanner (ScannerState -> (a, ScannerState))
:: ScannerState =	{ buffer	:: String
					, pos		:: Position
//					, tokens	:: [Token] 
					, log		:: [Error]}


/*** Monadic stuff ***/

instance Functor		Scanner where
	fmap :: (a -> b) (Scanner a) -> (Scanner b)
	fmap f (Scanner scan) = Scanner (\state ->
				let (a, state2) = scan state
				in (f a, state2))

instance Applicative	Scanner where
	pure  :: a -> Scanner a
	pure a = Scanner \st -> (a, st)
	(<*>) :: (Scanner (a -> b)) (Scanner a) -> Scanner b
	(<*>) (Scanner sf) (Scanner s) = Scanner \st -> 
				let
					(f, st2) = sf st
					(a, st3) = s st2
				in  (f a, st3)

instance Monad			Scanner where
	bind :: (Scanner a) (a -> Scanner b) -> Scanner b
	bind (Scanner s) f = Scanner \st ->
				let
					(a, st2) = s st
					(Scanner scan) = f a
				in scan st2

/*** Other tooling ***/
hasNext :: Scanner Bool
hasNext = Scanner \st -> (not (st.buffer == ""), st)

read	:: Scanner (Maybe Char)
read = Scanner scan
where
	scan st = if (st.buffer == "")
		(Nothing, st)
		(let (char, tail) = uselect st.buffer 0
		 in (Just char, {st & buffer = tail
							, pos    = nextPos st.pos char})
		)

peek	:: Scanner (Maybe Char)
peek = Scanner scan
where
	scan st = if (st.buffer == "")
		(Nothing, st)
		(Just (select st.buffer 0), st)

getPos  :: Scanner Position
getPos = Scanner \st -> (st.pos, st)

log		:: Error			-> Scanner ()
log e = Scanner \st -> ((), {st & log = [e: st.log]})

logHere :: Severity String	-> Scanner ()
logHere sev str = getPos >>= \pos -> log (Error pos sev str)

runScanner :: String (Scanner a) -> (a, [Error])
runScanner initStr (Scanner scan)
# startState =
	{ buffer	= initStr
	, pos		= zero
	, log		= []
	}
# (a, st) = scan startState
= (a, st.log)





Start = 0