definition module StringScanner

import Control.Monad

:: Scanner a

instance Functor		Scanner
instance Applicative	Scanner
instance Monad			Scanner

read :: Scanner a
peek :: Scanner a

initScanner :: String -> Scanner Token