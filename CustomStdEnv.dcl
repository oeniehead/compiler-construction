definition module CustomStdEnv

import 
	StdBool,
	StdInt,
	StdReal,
	StdChar,
//	StdArray, name clash met Data.Set, importeer waar nodig
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

import StdMaybe