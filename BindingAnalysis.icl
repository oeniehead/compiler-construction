implementation module BindingAnalysis

import AST
import Parser

import Data.Functor
import Control.Applicative
import Control.Monad

import StdGeneric
import GenString
import Error
import StdList
from Data.Func import $

import StdDebug

import Data.Either
import Data.Graph
import Data.Map

derive gString OrderItem, Graph, Map, Node, ()

:: BDMonad a = BD (BDMonadState -> (Maybe a, BDMonadState))
:: BDMonadState = {
		localVariables 	:: [String]	// Local variables	
	,	definitions		:: Map OrderItem Decl
	,	errors			:: [Error]	// To keep track of errors
	,	me 				:: OrderItem 	// Current scope
	,	stack			:: [OrderItem] // Stack
	,	depends			:: Graph OrderItem () // Graph to hold dependencies
	,	lookup			:: Map OrderItem NodeIndex
	}
	
instance zero BDMonadState
	where 
		zero = {
				//globalVariables = [],
				localVariables = [],
				//functions = [],
				definitions = newMap,
				errors = [],
				me = Global,
				stack = [],
				depends = emptyGraph,
				lookup = newMap
			}
	
:: OrderItem
	= VarItem String
	| FuncItem String
	| Combined [OrderItem]
	| Global
	
instance < OrderItem where
	(<) a b = (gString{|*|} a) < (gString{|*|} b)
	
uptoBinding ::
	String
	([Error] -> Maybe a)
	([Error] -> a)
	([Error] -> Maybe a)
	([Error] -> a)
		-> Either a (AST, [Error])
uptoBinding prog fscanErrors fparseFail fparseErrors fbindingErrors =
	case uptoParse prog fscanErrors fparseFail fparseErrors of
		Left a	= Left a
		Right (ast, parseErrors) =
			let result = doBindingAnalysis ast
			in case result of
				Left errors = Left $ fbindingErrors (parseErrors ++ errors)
				Right (ast, errors) = Right (ast, parseErrors ++ errors)
				
doBindingAnalysis :: AST -> Either [Error] (AST, [Error])
doBindingAnalysis ast = 
	let
		(BD f) = 
				process ast
			>>| checkVariableCycles
			>>| orderAST
		(res, st) = f zero
	in	case res of
			(Nothing) 	= Left st.errors
			(Just ast)	= Right (ast, st.errors)
	
bdAp :: (BDMonad (a -> b)) (BDMonad a) -> BDMonad b
bdAp (BD bdf) (BD bda) = BD \st.
	case bdf st of
		(Just f, st2)	= case bda st2 of
				(Just a, st3)	= (Just (f a), st3)
				(Nothing,st3)	= (Nothing, st3)
		(Nothing, st2)	= (Nothing, st2)

instance Monad BDMonad where
	bind (BD bda) f = BD \st.
		case bda st of
			(Just a, st2)	= let (BD bdb) = f a in bdb st2
			(Nothing,st2)	= (Nothing,st2)
instance Applicative BDMonad where
	pure a = BD \st.(Just a,st)
	(<*>) bdf bda = bdAp bdf bda
instance Functor BDMonad where
	fmap f bda = bdAp (return f) bda

error :: Error -> BDMonad a
error e = BD \st. (Nothing, { st & errors = [e : st.errors]})

debug :: Error -> BDMonad ()
debug e = return ()//BD \st. (Just (), { st & errors = st.errors ++ [e]})

/*registerGlobalVariable :: String -> BDMonad ()
registerGlobalVariable name = 
	BD \st. (Just (), {st & globalVariables = [name : st.globalVariables]})*/
		
registerLocalVariable :: String -> BDMonad ()
registerLocalVariable name = 
	debug {
		pos = zero,
		severity = DEBUG,
		stage = Binding,
		message = "Registering local variable " +++ name
	}
	>>| 
		BD \st. case length (filter (\n. n == name) st.localVariables) of
			(0) = (Just (), {st & localVariables = [name : st.localVariables]})
			_	= (Nothing, {st & errors = st.errors ++ [
						{
							pos = zero,
							severity = FATAL,
							stage = Binding,
							message = "Variable " +++ name +++ " is already defined"
						}
					]
				})
			
getLocalVariables :: (BDMonad [String])
getLocalVariables = 
	BD \st. (Just st.localVariables, st)
		
define :: OrderItem Decl -> BDMonad ()
define item decl = 
	BD \st. case get item st.definitions of
				(Nothing) = (Just (), {st & definitions = put item decl st.definitions})
				_	= (Nothing, {st & errors = st.errors ++ [
							{
								pos = zero,
								severity = FATAL,
								stage = Binding,
								message = (gString {|*|} item) +++ " is already defined"
							}
						]
					})
					
getDefinitions :: BDMonad (Map OrderItem Decl)
getDefinitions = BD \st.
	(Just st.definitions, st)
		
getDefinition :: OrderItem -> BDMonad Decl
getDefinition item = 
		getDefinitions >>= \definitions.
		case get item definitions of
			(Just decl) = return decl
			(Nothing) = error {
								pos = zero,
								severity = FATAL,
								stage = Binding,
								message = (gString {|*|} item) +++ " is not defined"
							}
							
getNodeDefinition :: NodeIndex (Graph OrderItem ()) -> BDMonad [Decl]
getNodeDefinition index graph = let
			item = fromJust (getNodeData index graph)
			definitions = case item of
				(Combined list) = resolveList list
				other = getDefinition other >>= \def. return [def]
		in definitions
where 
	resolveList :: [OrderItem] -> BDMonad [Decl]
	resolveList [] = return []
	resolveList [a : b] = 
		resolveList b >>= \others.
		getDefinition a >>= \item. return [item : others]
	
	
leave :: BDMonad ()
leave = BD \st.
	(Just (), {st & localVariables = [], me = (hd st.stack), stack = (tl st.stack)})
	
enter :: OrderItem -> (BDMonad ())
enter item = 
	debug {
			pos = zero,
			severity = DEBUG,
			stage = Binding,
			message = "Entering " +++ (gString{|*|} item)
		}
	>>| BD \st.
			(Just (), {st & localVariables = [], me = item, stack = [st.me : st.stack]})

getOrAddNode :: OrderItem -> (BDMonad NodeIndex)
getOrAddNode item = BD \st.
	let	(index, depends) = case get item st.lookup of
			(Just index) = (index, st.depends)
			(Nothing) = addNode item st.depends
		lookup = put item index st.lookup
	in	(Just index, {st & depends = depends, lookup = lookup})
	
addDependency :: NodeIndex NodeIndex -> (BDMonad ())
addDependency indexA indexB = BD \st.
	(Just (), {st &
		depends = addEdge () (indexA, indexB) st.depends})
		
getMe :: BDMonad OrderItem
getMe = BD \st. (Just st.me, st)

depends :: OrderItem -> (BDMonad ())
depends item = 
	getMe 				>>= \me.
	getOrAddNode me 	>>= \indexA.
	getOrAddNode item	>>= \indexB.
	debug {
			pos = zero,
			severity = DEBUG,
			stage = Binding,
			message = (gString{|*|} me) +++ " relies on " +++ (gString {|*|} item)
		} >>|
	addDependency indexA indexB
	
unFieldId :: IdWithFields -> (BDMonad Id)
unFieldId (WithField field _ _) = unFieldId field
unFieldId (JustId id _) = return id
	
forGlobalVariable :: Id (BDMonad ()) -> (BDMonad ())
forGlobalVariable name func = 
	getLocalVariables >>= \locals.
	debug {
			pos = zero,
			severity = DEBUG,
			stage = Binding,
			message = "Current locals: " +++ (gString{|*|} locals)
		}
	>>| 
	BD \st.
	if (length (filter (\n. n == name) st.localVariables) <> 0)
		(Just (), st)
		(let
			(BD f) = func
		in f st)
			
getDepends :: (BDMonad (Graph OrderItem ()))
getDepends = BD \st. (Just st.depends, st)

setDepends :: (Graph OrderItem ()) -> BDMonad ()
setDepends graph = BD \st.
	(Just (), {st & depends = graph})

getLookup :: (BDMonad (Map OrderItem NodeIndex))
getLookup = BD \st. (Just st.lookup, st)

lookupItem :: OrderItem -> (BDMonad NodeIndex)
lookupItem item = 
		getLookup >>= \lookup.
		BD \st.
			case get item lookup of
				(Just index) = (Just index, st)
				(Nothing) = (Nothing, {st & errors = st.errors ++ [
									{
										pos = zero,
										severity = FATAL,
										stage = Binding,
										message = (gString{|*|} item) +++ " does not exist"
									}
								]
							})
			
spliceMainComponent :: (BDMonad ())
spliceMainComponent = lookupItem (FuncItem "main") >>| return ()
		/*lookupItem (FuncItem "main") >>= \mainIndex.
			(BD \st. 
				let
					parts = components st.depends
					mainComponent = hd (filter (\graph. isJust (getNodeData mainIndex graph)) parts)
				in	(Just (), {st & depends = mainComponent}))*/

findCycle :: (BDMonad (Maybe [NodeIndex]))
findCycle = getDepends >>= \depends.
		let indices = nodeIndices depends
			maxDepth = (mapSize depends.nodes) + 1
			cycle = foldl (localFindCycle maxDepth depends) Nothing indices
		in 	return cycle
	where 
		localFindCycle :: Int (Graph OrderItem ()) (Maybe [NodeIndex]) NodeIndex -> Maybe [NodeIndex]
		localFindCycle 0 _ _ _ = Nothing
		localFindCycle _ _ (Just path) _ = (Just path)
		localFindCycle depth graph _ start = let
				sucessors = directSuccessors start graph
				path = foldl (localFindPath start (depth - 1) graph) Nothing sucessors
			in case path of
				(Nothing) = Nothing
				(Just p) = Just [start : p]
		localFindPath :: NodeIndex Int (Graph OrderItem ()) (Maybe [NodeIndex]) NodeIndex -> (Maybe [NodeIndex])
		localFindPath _ _ _ (Just path) _ = (Just path)
		localFindPath end depth graph (Nothing) start
		| start == end = Just []
		| depth == 0 = Nothing
		| otherwise = let
				sucessors = directSuccessors start graph
				path = foldl (localFindPath end (depth - 1) graph) Nothing sucessors
			in case path of
				(Nothing) = Nothing
				(Just p) = Just [start : p]

mergePath :: [NodeIndex] -> BDMonad ()
mergePath [] = return ()
mergePath [a : []] = return ()
mergePath [into : others] = getDepends >>= \depends.
	let
		intoItem = Combined [fromJust (getNodeData into depends)]
		graph = foldl (mergeInto into) (setNodeData into intoItem depends) others
	in	setDepends graph
where
	mergeInto :: NodeIndex (Graph OrderItem ()) NodeIndex -> (Graph OrderItem ())
	mergeInto goal graph item = let
			predecessorEdges = getPredecessorEdgesFromNode item graph
			graph2 = movePredecessors predecessorEdges goal graph
			successorEdges = getSuccessorEdgesFromNode item graph2
			graph3 = moveSuccessors successorEdges goal graph2
			graph4 = mergeItems goal item graph3
			graph5 = removeNode item graph4
			graph6 = removeEdge (goal, goal) graph5
		in graph6
	getPredecessorEdgesFromNode :: NodeIndex (Graph OrderItem ()) -> [EdgeIndex]
	getPredecessorEdgesFromNode index graph =
		filter (\(_, x). x == index) (keys graph.edges)
	movePredecessors :: [EdgeIndex] NodeIndex (Graph OrderItem ()) -> (Graph OrderItem ())
	movePredecessors [] _ graph = graph
	movePredecessors edges goal graph = 
		foldl (\graph index=:(a, b). let
					graph2 = removeEdge index graph
					graph3 = addEdge () (a, goal) graph2
				in graph3	
			) graph edges
	getSuccessorEdgesFromNode :: NodeIndex (Graph OrderItem ()) -> [EdgeIndex]
	getSuccessorEdgesFromNode index graph =
		filter (\(x, _). x == index) (keys graph.edges)
	moveSuccessors :: [EdgeIndex] NodeIndex (Graph OrderItem ()) -> (Graph OrderItem ())
	moveSuccessors [] _ graph = graph
	moveSuccessors edges goal graph = 
		foldl (\graph index=:(a, b). let
					graph2 = removeEdge index graph
					graph3 = addEdge () (goal, b) graph2
				in graph3
			) graph edges
	mergeItems :: NodeIndex NodeIndex (Graph OrderItem ()) -> (Graph OrderItem ())
	mergeItems goal item graph = 
		let	(Combined goalItems) = fromJust (getNodeData goal graph)
			orderItem = fromJust (getNodeData item graph)
		in 	setNodeData goal (Combined [orderItem : goalItems]) graph

resolveCycles :: BDMonad ()
resolveCycles = 
		findCycle >>= \mCycle.
		case mCycle of
			(Nothing) = return ()
			(Just cycle) = mergePath cycle >>| resolveCycles
			
checkVariableCycles :: BDMonad ()
checkVariableCycles = 
		getDepends >>= \graph.
		sequence_ (map (
				\node. let
					item = node.data
				in verifyOrderItem item
			) (elems graph.nodes)
		)
where
	verifyOrderItem :: OrderItem -> BDMonad ()
	verifyOrderItem (VarItem _) = return ()
	verifyOrderItem (FuncItem _) = return ()
	verifyOrderItem (Combined list) = sequence_ (
			map (disallowVars) list
		)
	disallowVars :: OrderItem -> BDMonad ()
	disallowVars (VarItem name) = error {
										pos = zero,
										severity = FATAL,
										stage = Binding,
										message = "Cyclic dependency on variable " +++ name
									}
	disallowVars _ = return ()

/*
	Build the new AST. Just keep taking all leaf nodes from the graph and adding them to the AST.
*/
orderAST :: BDMonad AST
orderAST = getDepends >>= \graph.
		buildAST graph >>= \ast.
		return ast
where
	buildAST :: (Graph OrderItem ()) -> BDMonad AST
	buildAST graph = case isEmptyGraph graph of
		(True) = return []
		(False) = (BD \st. (Just (leafNodes graph), st)) 
			>>= \sinks. retrieveItems sinks graph
			>>= \decls. removeItems sinks graph
			>>= \newGraph. buildAST newGraph
			>>= \items. return (decls ++ items)
	retrieveItems :: [NodeIndex] (Graph OrderItem ()) -> BDMonad AST
	retrieveItems [] _ = return []
	retrieveItems [a : b] graph = 
		retrieveItems b graph >>= \others.
		getNodeDefinition a graph >>= \items.
			return (items ++ others)
	removeItems :: [NodeIndex] (Graph OrderItem ()) -> BDMonad (Graph OrderItem ())
	removeItems items graph = return (foldl (\graph node. removeNode node graph) graph items)
		

class process a :: a -> BDMonad ()

instance process AST where
	process ast = sequence_ (
					map (process) ast
				)
			/*>>| (BD \st.
					let
						str = gString{|*|} st.depends//foldlNodes (\s index node. s +++ (gString{|*|} node.data)) "" st.depends
					in	(Just (), {st & errors = [{
							pos = zero,
							severity = DEBUG,
							stage = Binding,
							message = str
						} : st.errors]})
				)*/
			>>| spliceMainComponent
			/*>>| (BD \st.
					let
						str = gString{|*|} st.depends//foldlNodes (\s index node. s +++ (gString{|*|} node.data)) "" st.depends
					in	(Just (), {st & errors = [{
							pos = zero,
							severity = DEBUG,
							stage = Binding,
							message = str
						} : st.errors]})
				)*/
			>>| resolveCycles
			/*>>| (BD \st.
					let
						str = gString{|*|} st.depends//foldlNodes (\s index node. s +++ (gString{|*|} node.data)) "" st.depends
					in	(Just (), {st & errors = [{
							pos = zero,
							severity = DEBUG,
							stage = Binding,
							message = str
						} : st.errors]})
				)*/
			
instance process Decl where
	process decl=:(Var varDecl=:(VarDecl _ id _ _)) = 
			define (VarItem id) decl
		>>| process varDecl
	process decl=:(Fun funDecl=:(FunDecl id _ _ _ _ _)) = 
			define (FuncItem id) decl
		>>| process funDecl
	
instance process VarDecl where
	process (VarDecl mType id expr _) =
			enter (VarItem id)
		>>|	process expr
		>>| leave
		
instance process FunDecl where
	process (FunDecl id aArg mType decls stmts _) =
		let
			processVariables = sequence_ (
					map (
						\(VarDecl _ name expr _).
								process expr
							>>| registerLocalVariable name
					) decls
				)
			
		in	enter (FuncItem id)
		>>| getOrAddNode (FuncItem id)
		>>| sequence_ (
			map (registerLocalVariable) aArg
		)
		>>| processVariables
		>>| process stmts
		>>| leave

instance process [a] | process a 
	where
		process stmts = sequence_ (
				map (process) stmts
			)
		
instance process Stmt where
	process (StmtIf cond expr mExpr _) = 
		let
			processBExpr = case mExpr of
				(Nothing) = return ()
				(Just ex) = process ex
		in	process cond
		>>| process expr
		>>| processBExpr
	process (StmtWhile cond stmts _) =
			process cond
		>>| process stmts
	process (StmtAss fields expr _) =
			unFieldId fields
		>>= \name.
			forGlobalVariable name (
				depends (VarItem name)
			)
		>>|	process expr
	process (StmtFunCall funCall _) =
		process funCall
	process (StmtRet expr _) =
		process expr
	process (StmtRetV _) = 
		return ()
		
instance process Expr where
	process (ExpIdent field _) =
			unFieldId field
		>>= \name.
			forGlobalVariable name (
				depends (VarItem name)
			)
	process (ExpBinOp exprA _ exprB _) =
			process exprA
		>>| process exprB
	process (ExpUnOp _ expr _) = 
		process expr
	process (ExpInt _ _) = 
		return ()
	process (ExpChar _ _) = 
		return ()
	process (ExpBool _ _) =
		return ()
	process (ExpFunCall funCall _) =
		process funCall
	process (ExpEmptyArray _) =
		return ()
	process (ExpTuple exprA exprB _) =
			process exprA
		>>| process exprB
		
instance process FunCall where
	process (FunCall name args _) =
			if (name == "isEmpty" || name == "print" || name == "read")
			(return ())
			(depends (FuncItem name))
		>>| process args






















