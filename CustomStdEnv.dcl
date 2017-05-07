definition module CustomStdEnv

import 
	StdBool,
	StdInt,
	StdReal,
	StdChar,
//	StdArray, //name clash with Data.Set(size)
	StdString,
	StdFile,

	StdClass,

//	StdList, //name clash with Data.Set(insert and filter)
	StdOrdList,
	StdTuple,
	StdCharList,
//	StdFunc, //name clash with return
	StdMisc,

	StdEnum

from StdList import
	instance ==	[a] | == a,
	instance <	[a] | Ord a,
	instance length	[],
	instance %		[a],
	instance toString 	[x] | toChar x,
	instance fromString [x] | fromChar x,
	++, flatten, isEmpty, hd, tl, last, init,
	take, takeWhile, drop, dropWhile, span, reverse, insertAt, removeAt, updateAt, splitAt,
	map, iterate, indexList, repeatn, repeat, unzip, zip2, zip, diag2, diag3,
	foldl, foldr, scan, and, or, any, all,
	isMember, isAnyMember, removeMember, removeMembers, removeDup, removeIndex, limit,
	sum, prod, avg

from StdFunc import id, const, o, flip

import StdMaybe