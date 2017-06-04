implementation module BindingAnalysis

import SPLParser

import StdGeneric
import GenString
import Error
import StdList
from Data.Func import $

import StdDebug

import Data.Either

:: BAMonad a = BD (BAMonadState -> (Maybe a, BAMonadState))
:: BAMonadState = {
	,	globalVariables :: [String] // Global variables
	,	localVariables 	:: [String]	// Local variables	
	,	errors			:: [Error]	// To keep track of errors
	}

error :: Error -> CGMonad ()
error e = CG \st. (Nothing, { st & errors = [e : st.errors]})


/*derive gString OrderItem, Error, Signature
derive gEq OrderItem
instance toString OrderItem where toString x = gString{|*|} x
instance toString OrderGraph where toString x = gString{|*|} x
instance toString (OrderItem, OrderItem) where toString x = gString{|*|} x

instance == OrderItem where
	(==) (VarItem name _) (VarItem name2 _) = name == name2
	(==) (FuncItem name _) (FuncItem name2 _) = name == name2
	(==) (CallsItem name _ _ _) (CallsItem name2 _ _ _) = name == name2
	(==) (FuncItem name _) (CallsItem name2 _ _ _) = name == name2
	(==) (CallsItem name _ _ _) (FuncItem name2 _) = name == name2
	(==) _ _ = False

buildInFunctions :: [String]
buildInFunctions = [
		"isEmpty",
		"print",
		"read"
	]

:: ReturnBehaviour
	= Sometimes Bool
	| Always	Bool
	| Never
	
:: ReturnResult :== (Either ReturnBehaviour Error)

uptoBinding ::
	String
	([Error] -> Maybe a)
	([Error] -> a)
	([Error] -> a)
		-> Either a (AST, [Error])
uptoBinding prog fscanErrors fparseErrors fbindingErrors =
	case uptoParse prog fscanErrors fparseErrors of
		Left a	= Left a
		Right (ast, parseErrors) =
			let result = doBindingAnalysis ast
			in case result of
				Left errors = Left $ fbindingErrors (parseErrors ++ errors)
				Right ast = Right (ast, parseErrors ++ errors)


doAnalysis :: OrderGraph AST -> OrderGraph
doAnalysis _ [] = ([], [], [])
doAnalysis graph [declaration : declarations] = 
	let item =  buildGraph graph declaration
		items = combineGraphs item graph
	in	combineGraphs items (doAnalysis items declarations)
	
buildGraph :: OrderGraph Decl -> OrderGraph
buildGraph g (Var v) = doOrder g v
buildGraph g (Fun f) = doOrder g f

combineGraphs :: OrderGraph OrderGraph -> OrderGraph
combineGraphs (a, b, c) (i, j, k) = (a ++ i, b ++ j, c ++ k)
	
/*
Build graph from VarDecl:
	- Parse expression tree without local variables
	- If the variable is used inside its own initialisation a cycle will be detected
*/
instance doOrder VarDecl where
	doOrder graph (VarDecl _ name expression {MetaData| pos = pos, type = _}) = 
		let	me 		= (VarItem name pos)
			item 	= if dup 
						([], [], [{
									pos = pos,
									severity = FATAL,
									stage = Binding,
									message = "Variable " +++ name +++ " is defined more than once"
								}])
						([], [signature], [])
		in 	combineGraphs (buildGraphFrom me [] expression) item
		where
			signature = VarSig name
			dup = isDuplicate graph signature

/*
Build graph from FunDecl:
	- For each VarDecl build a graph with the previous variables as locals
	- Build graph for all Stmts with all local variables
	- Recurse through all statements to detect if every branch either
	  returns something or nothing
	- A cycle in functions means recursion, for now we do not allow that
*/
instance doOrder FunDecl where
	doOrder graph (FunDecl name arguments type variables statements {MetaDataTS| pos = pos, typeScheme = _}) = 
		let 
			me = FuncItem name pos
			returns = returnsValue pos statements
			function_dup = passThroughError (isDuplicate graph (FuncSig name 0 False)) {
															pos = pos,
															severity = FATAL,
															stage = Binding,
															message = "Function " +++ name +++ " is defined more than once"
														} 
			arguments_dup = findDuplicates arguments pos
			local_dup = findDuplicates localVariables pos
			local_errors = function_dup ++ arguments_dup ++ local_dup
			item = case returns of
				(Right error) 	= ([], [], [error] ++ local_errors)
				(Left retv)		= ([], [(FuncSig name (length arguments) retv)], local_errors)
		in	combineGraphs (buildGraphFrom me localEnv statements) item
		where
			localVariables = getLocalVariableNames variables
			localEnv = arguments ++ localVariables
			getLocalVariableNames :: [VarDecl] -> [String]
			getLocalVariableNames declarations = map (\(VarDecl _ name _ _). name) declarations
			getVariableGraph :: [VarDecl] OrderItem [String] -> OrderGraph
			getVariableGraph [] _ _ = ([], [])
			getVariableGraph [(VarDecl _ name expression _) : b] me localEnv = 
				combineGraphs 	(buildGraphFrom me localEnv expression)
								(getVariableGraph b me [name : localEnv])
			passThroughError :: Bool Error -> [Error]
			passThroughError True error = [error]
			passThroughError False _ = []
			returnsValue :: Position [Stmt] -> (Either Bool Error) 
			returnsValue pos stmts =
				case sum of
					(Left b) 		= case b of
							(Always returns) 	= (Left returns)
							(Never)				= (Left False)
							(Sometimes False)	= (Left False)
							(Sometimes True) 	= (Right {
															pos = pos,
															severity = FATAL,
															stage = Binding,
															message = "Function must either always or never return something"
														})
					(Right error) = (Right error)
				where sum = summariseStatements stmts pos
				
findDuplicates :: [String] Position -> [Error]
findDuplicates [] _ = []
findDuplicates [a : b] pos = 
	if (isMember a b)
		(error ++ next)
		next
	where
		next = findDuplicates b pos
		error = [{
					pos = pos,
					severity = FATAL,
					stage = Binding,
					message = "Variable " +++ a +++ " is defined multiple times"
			}]	
				
isDuplicate :: OrderGraph Signature -> Bool
isDuplicate (_, signatures, _) sig = length (filter (matchSignatures sig) signatures) > 0

matchSignatures :: Signature Signature -> Bool
matchSignatures (VarSig a) (VarSig b) = a == b
matchSignatures (FuncSig a _ _) (FuncSig b _ _) = a == b
matchSignatures _ _ = False

/*
:: Stmt
	= StmtIf Expr [Stmt] (Maybe [Stmt])	MetaData
	| StmtWhile Expr [Stmt]				MetaData
	| StmtAss IdWithFields Expr			MetaData
	| StmtFunCall FunCall				MetaData
	| StmtRet Expr						MetaData
	| StmtRetV							MetaData
*/
instance buildGraphFrom Stmt where
	buildGraphFrom me local (StmtIf exp_c stmts Nothing _) = 
		combineGraphs	(buildGraphFrom me local exp_c)
						(buildGraphFrom me local stmts)
	buildGraphFrom me local (StmtIf exp_c stmts (Just stmts_else) _) = 
		combineGraphs 		(buildGraphFrom me local exp_c) (
			combineGraphs 	(buildGraphFrom me local stmts)
							(buildGraphFrom me local stmts_else)
		)
	buildGraphFrom me local (StmtWhile exp_c stmts _) = 
		combineGraphs 	(buildGraphFrom me local exp_c)
						(buildGraphFrom me local stmts)
	buildGraphFrom me local (StmtAss idWithFields exp _) = 
		combineGraphs 	(buildGraphFrom me local idWithFields)
						(buildGraphFrom me local exp)
	buildGraphFrom me local (StmtFunCall (FunCall func args {MetaData| pos = pos, type = _}) _) = 
		let exprs = map (buildGraphFrom me local) args
			this = if (isMember func buildInFunctions) ([], [], []) ([(me, (CallsItem func (length args) False pos))], [], [])
		in 	foldl (combineGraphs) ([], [], []) [this : exprs]
	buildGraphFrom me local (StmtRet exp _) = 
		buildGraphFrom me local exp
	buildGraphFrom me local (StmtRetV _) = ([], [], [])

instance buildGraphFrom [Stmt] where
	buildGraphFrom me local stmts = 
		let [item : items] = map (buildGraphFrom me local) stmts
			graph = foldl (combineGraphs) item items
		in	graph

/*
:: Expr
	= ExpIdent IdWithFields		MetaData
	| ExpBinOp Expr BinOp Expr	MetaData
	| ExpUnOp UnOp Expr			MetaData
	| ExpInt Int				MetaData
	| ExpChar Char				MetaData
	| ExpBool Bool				MetaData
	| ExpFunCall FunCall		MetaData
	| ExpEmptyArray				MetaData
	| ExpTuple Expr Expr		MetaData
*/
instance buildGraphFrom Expr where
	buildGraphFrom me local (ExpIdent idWithFields				_) = buildGraphFrom me local idWithFields
	buildGraphFrom me local (ExpBinOp exp_a _ exp_b				_) = combineGraphs (buildGraphFrom me local exp_a) (buildGraphFrom me local exp_b)
	buildGraphFrom me local (ExpUnOp _ exp_a					_) = buildGraphFrom me local exp_a
	buildGraphFrom me local (ExpInt  _							_) = ([], [], [])
	buildGraphFrom me local (ExpChar _							_) = ([], [], [])
	buildGraphFrom me local (ExpBool _							_) = ([], [], [])
	buildGraphFrom me local (ExpFunCall (FunCall func args {MetaData| pos = pos, type = _})	_) = 
		let exprs = map (buildGraphFrom me local) args
			this = if (isMember func buildInFunctions) ([], [], []) ([(me, (CallsItem func (length args) True pos))], [], [])
		in 	foldl (combineGraphs) ([], [], []) [this : exprs]
	buildGraphFrom me local (ExpEmptyArray						_) = ([], [], [])
	buildGraphFrom me local (ExpTuple exp_a exp_b				_) = combineGraphs (buildGraphFrom me local exp_a) (buildGraphFrom me local exp_b)
	
/*
:: IdWithFields
	= WithField IdWithFields Field		MetaData
	| JustId	Id						MetaData
*/
instance buildGraphFrom IdWithFields where
	buildGraphFrom me local (WithField idWithFields _ 	_) = buildGraphFrom me local idWithFields
	buildGraphFrom me [] (JustId id 				  	{MetaData| pos = pos, type = _}) = ([(me, (VarItem id pos))], [], [])
	buildGraphFrom me [a:rest] item =:(JustId id 		_)
	| a == id 	= ([], [], [])
	| otherwise = buildGraphFrom me rest item
	
getReturnBehaviour :: Stmt -> ReturnResult
getReturnBehaviour (StmtIf exp_c stmts Nothing 				{MetaData| pos = pos}) = 
	let ret = summariseStatements stmts pos
	in	case ret of
		(Right error) = (Right error)
		(Left res) = case res of
			(Always r)		= (Left (Sometimes r))
			(Sometimes r) 	= (Left (Sometimes r))
			(Never)			= (Left (Never))
getReturnBehaviour (StmtIf exp_c stmts (Just stmts_else) 	{MetaData| pos = pos}) = 
	let	a = summariseStatements stmts pos
		b = summariseStatements stmts_else pos
	in	combineEitherStatements pos a b
getReturnBehaviour (StmtWhile exp_c stmts 					{MetaData| pos = pos}) = summariseStatements stmts pos
getReturnBehaviour (StmtAss idWithFields exp 				{MetaData| pos = pos}) = (Left (Never))
getReturnBehaviour (StmtFunCall (FunCall func args _)  		{MetaData| pos = pos}) = (Left (Never))
getReturnBehaviour (StmtRet exp  							{MetaData| pos = pos}) = (Left (Always True))
getReturnBehaviour (StmtRetV  								{MetaData| pos = pos}) = (Left (Always False))

summariseStatements :: [Stmt] Position -> ReturnResult
summariseStatements stmts pos = 
	let results 		= map (getReturnBehaviour) stmts
		return_results 	= reverse results 
	in	combineStatements return_results pos
	
combineStatements :: [ReturnResult] Position -> ReturnResult
combineStatements [result: []] pos = result
combineStatements [result : results] pos =
	let next = combineStatements results pos
		combined = case next of
			(Right error) 		= (Right error)
			(Left behaviour)	= case result of
				(Right error)	= (Right error)
				(Left a)		= case behaviour of
					(Always i) = case a of
							(Always j) 		= compareReturns pos i j (Always (i || j))
							(Sometimes j) 	= compareReturns pos i j (Always (i || j))
							(Never)			= next
					(Sometimes i) = case a of
							(Always j) 		= compareReturns pos i j (Always (i || j))
							(Sometimes j) 	= compareReturns pos i j (Sometimes (i || j))
							(Never)			= next
					(Never) = case a of
							(Always j) 		= (Left (Always j))
							(Sometimes j) 	= (Left (Sometimes j))
							(Never)			= next
	in combined

combineEitherStatements :: Position ReturnResult ReturnResult -> ReturnResult
combineEitherStatements pos a b =
	case a of
		(Right error) = a
		(Left when_a) = 
			case b of
				(Right error) = b
				(Left when_b) =
					case when_a of
						(Always ret_a) = 
							case when_b of
								(Always ret_b) 		= compareReturns pos ret_a ret_b (Always (ret_a || ret_b)) 
								(Sometimes ret_b) 	= compareReturns pos ret_a ret_b (Sometimes (ret_a || ret_b)) 
								(Never) 			= (Left (Sometimes ret_a))
						(Sometimes ret_a) = 
							case when_b of
								(Always ret_b) 		= compareReturns pos ret_a ret_b (Always (ret_a || ret_b))
								(Sometimes ret_b)	= compareReturns pos ret_a ret_b (Sometimes (ret_a || ret_b))
								(Never)				= (Left (Sometimes ret_a))
						(Never) = 
							case when_b of
								(Always ret_b)		= (Left (Sometimes ret_b))
								(Sometimes ret_b)	= (Left (Sometimes ret_b))
								(Never)				= (Left (Never))
		
compareReturns :: Position Bool Bool ReturnBehaviour -> ReturnResult
compareReturns pos a b res
	| a == b 	= (Left res)
	| otherwise = (Right {
		pos = pos,
		severity = FATAL,
		stage = Binding,
		message = "Return type confusion (Void/Type)"
	})
	
doUnusedAnalysis :: OrderGraph -> [Error]
doUnusedAnalysis ([], signatures, errors) = []
doUnusedAnalysis ([(a, b): relations], signatures, errors) =
	let pos = case b of
				(VarItem _ p) = p
				(CallsItem _ _ _ p) = p
		name = case b of
				(VarItem name _) = "Variable " +++ name
				(CallsItem name args returns _) = "Function(" +++ (toString args) +++ ")(" +++ toString returns +++ ") " +++ name
		next = doUnusedAnalysis (relations, signatures, errors)
		in if (itemExists b signatures)
					(next) ([{Error |
							pos = pos,
							severity = FATAL,
							stage = Binding,
							message = name +++ " is not defined."
						} : next]) 
		
itemExists :: OrderItem [Signature] -> Bool
itemExists inp=:(VarItem name _) [] 				= False
itemExists inp=:(CallsItem name _ _ _) [] 			= False
itemExists inp=:(VarItem name _) [(VarSig signame): b]
| name == signame = True
| otherwise = itemExists inp b
itemExists inp=:(CallsItem name use_args use_return _) [(FuncSig signame fun_args fun_return): b]
| name <> signame = itemExists inp b
| use_args <> fun_args = False
| use_return <> fun_return = False
| otherwise = True
itemExists inp [_: b] = itemExists inp b

isVar :: OrderItem -> Bool
isVar (VarItem _ _) = True
isVar _	= False

doCycleAnalysis :: OrderGraph -> [Error]
doCycleAnalysis ([], signatures, errors) = []
doCycleAnalysis (items, _, _) = flatten [findCycle a items \\ (a, _) <- items | isVar a]

findCycle :: OrderItem [(OrderItem, OrderItem)] -> [Error]
findCycle current graph = 
	let tails 	= flatten [findTail current graph depth \\ depth <- [1 .. (length graph)]]
		cycles 	= filter ((==) current) tails
	in	map (\cycle. 
		let (name, pos) = case cycle of
					(VarItem n p) 	= ("variable " +++ n, p)
					(FuncItem n p) 	= ("function " +++ n, p)
		in	{
				pos = pos,
				severity = FATAL,
				stage = Binding,
				message = "Dependency cycle on " +++ name
			}
		) cycles
	
findTail :: OrderItem [(OrderItem, OrderItem)] Int -> [OrderItem]
findTail current _ 0 = [current]
findTail current graph depth = 
	let matching = filter (\(a, _). current == a) graph
		frontier	= map (\(_, b). convert b) matching
		next = map (\i. findTail i graph (depth - 1)) frontier
	in	flatten next
where
	convert :: OrderItem -> OrderItem
	convert (VarItem x p) = (VarItem x p)
	convert (CallsItem x _ _ p) = (FuncItem x p)
	
doOrderDeclarations :: OrderGraph AST -> AST
doOrderDeclarations (graph, _, _) ast = 
	let ordered = orderGraph graph
		dedup 	= deDuplicate ordered
		result 	= getOrderedAST ast dedup
	in	reverse result		

orderGraph :: [(OrderItem, OrderItem)] -> [(OrderItem, OrderItem)]
orderGraph [] = []
orderGraph graph = 
	let referenced 		= filter (\(a, _). isReferenced graph a) graph
		not_referenced 	= filter (\(a, _) . not (isReferenced graph a)) graph
	in	not_referenced ++ (orderGraph referenced)
	
isReferenced :: [(OrderItem, OrderItem)] OrderItem -> Bool
isReferenced graph item = length (filter (\(_, b). b == item) graph) > 0

deDuplicate :: [(OrderItem, OrderItem)] -> [OrderItem]
deDuplicate [] = []
deDuplicate [(item, _) : b] = 
	let rest = filter (\(a, _). a <> item) b
	in	[item : deDuplicate rest]

getOrderedAST :: AST [OrderItem] -> AST
getOrderedAST ast 	[] = ast
getOrderedAST ast 	[a : b] =
	let	pos = getASTPosition ast a
		item = ast !! pos
		rest = removeAt pos ast
	in	[item : getOrderedAST rest b]
	
getASTPosition :: AST OrderItem -> Int
getASTPosition [(Var (VarDecl _ name _ _)) : b] item=:(VarItem name2 _) = if (name == name2) (0) (1 + (getASTPosition b item))
getASTPosition [(Fun (FunDecl name _ _ _ _ _)) : b] item=:(FuncItem name2 _) = if (name == name2) (0) (1 + (getASTPosition b item))
getASTPosition [(Fun (FunDecl name _ _ _ _ _)) : b] item=:(CallsItem name2 _ _ _) = if (name == name2) (0) (1 + (getASTPosition b item))
getASTPosition [_ : b] item = 1 + (getASTPosition b item)

doBindingAnalysis :: AST -> Either [Error] AST
doBindingAnalysis ast =
	case (doAnalysis ([], [], []) ast) of
		graph=:(relations, signatures, []) = case doUnusedAnalysis graph of
			[] = case doCycleAnalysis graph of
				[] 	= (Right (doOrderDeclarations graph ast))
				errors = (Left errors)
			errors = (Left errors)
		(_, _, errors) = (Left errors)

*/




















