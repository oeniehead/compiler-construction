definition module Scanner

import Token

scanner :: String -> [Token]

branch1 :: Position -> Scanner Token
branch2 :: Position -> Scanner Token
branch3 :: Position -> Scanner Token
branch4 :: Position -> Scanner Token

readInteger :: String Position -> Scanner Token
readString  :: String Position -> Scanner Token