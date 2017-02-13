implementation module StringScanner

:: Scanner a = Scanner (ScannerState -> (ScannerState, a)) )
:: ScannerState =	{ buffer	:: String
					, linerNr	:: Int
					, colNr		:: Int
         			, tokens	:: [Token] 
         			, log		:: [Error])
return	:: a		-> Scanner a
log		:: Error	-> Scanner ()

instance Functor		Scanner where
	fmap :: (a -> b) (Scanner a) -> (Scanner b)
	fmap f (Scanner scan) = Scanner (\state ->
				let (state2,a) = scan state
				in (state2, f a))

instance Applicative	Scanner


instance Monad			Scanner

