implementation module TestTooling

//from gast import class gast
import MersenneTwister
import gen
import GenEq
import genLibTest
import testable
//import confSM
import stdProperty

// Other imports
import StdGeneric
from Data.Maybe import :: Maybe


derive bimap [] //Deze is nodig als je de error 'bimap_ss no instance availible of type _List' krijgt
derive ggen Maybe
derive genShow Maybe