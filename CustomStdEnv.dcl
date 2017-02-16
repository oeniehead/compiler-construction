definition module CustomStdEnv

import 
	StdBool,
	StdInt,
	StdReal,
	StdChar,
	StdArray,
	StdString,
	StdFile,

	StdClass,

	StdList,
	StdOrdList,
	StdTuple,
	StdCharList,
//	StdFunc, name clash met return
	StdMisc,

	StdEnum

from StdFunc import id, const, o, flip