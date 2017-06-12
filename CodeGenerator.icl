implementation module CodeGenerator

import AST

import Data.Functor
import Control.Applicative
import Control.Monad
import Data.Either

from StdList import filter

import qualified Data.Map as m

import StdGeneric
import GenString

import TypeChecker
from Data.Func import $

derive gString CGArg, CGInst, CGRegister

instance toString CGArg where toString x = gString{|*|} x
instance toString CGInst where toString x = gString{|*|} x

:: CGMonad a = CG (CGMonadState -> (Maybe a, CGMonadState))
:: CGMonadState = {
		instructions 	:: [CGInst]
		// These are simple lists, because the position of an item in the list
		// tells us the relative position, and that's all we need.
		// We need to reverse the order at lookup!
	,	globalVariables :: [String] // Global variables
	,	arguments 		:: [String]	// Function arguments
	,	localVariables 	:: [String]	// Local variables	
	,	errors			:: [Error]	// To keep track of errors
	,	currentFunction	:: String	// The name of the current function
	,	counter			:: Int		// Counter to generate unique labels
	}

:: CGArg
	= Val 	String	// Static value
	| Label String	// To the position of the label
	| Register CGRegister // A register identifier
	| Annote String // Annotation

:: CGInst
	= Inst String [CGArg] (Maybe String)

:: CGRegister
	= SP
	| MP
	| RR
	| PC
	| HP
	| BP

instance zero CGMonadState
	where 
		zero = {
				instructions = [],
				globalVariables = [],
				arguments = [],
				localVariables = [],
				errors = [],
				currentFunction = "_bootstrap",
				counter = 0
			}
			



/**
	Heap management:
		- Heap pointer HP points to next empty value on the heap.
		- Allocating data uses this pointer to find the new position.
		- After allocation, pointer is increased with the lengtht of the newly allocated data.
		- Items on the heap can be of any length.
		- Items on the heap are not tagged with their type.
		- Structure of items on the heap:
			a. Int: 	single 32-bit word
			b. Char: 	single 32-bit word
			c. Bool: 	single 32-bit word
			d. List:	2 32-bit words, pointed to by x on the stack.
				Layout:
					x - 1: 	Pointer to next list item, or 0 when this is the last.
					x:		Value of the list item.
				The empty list is defined by a 0/0 item.
			e: Tuple:	2 32-bit words, pointed to by x on the stack.
				Layout:
					x - 1:	Pointer to second item.
					x:		Pointer to first item.
					
	Stack management:
		- Stack begins with values of global variables.
		- Starting point of stack is after program code, so the total length
			of the program has to be known.
			(1st stack slot is at <program_length> + 16)
**/

cgAp :: (CGMonad (a -> b)) (CGMonad a) -> CGMonad b
cgAp (CG cgf) (CG cga) = CG \st.
	case cgf st of
		(Just f, st2)	= case cga st2 of
				(Just a, st3)	= (Just (f a), st3)
				(Nothing,st3)	= (Nothing, st3)
		(Nothing, st2)	= (Nothing, st2)

instance Monad CGMonad where
	bind (CG cga) f = CG \st.
		case cga st of
			(Just a, st2)	= let (CG cgb) = f a in cgb st2
			(Nothing,st2)	= (Nothing,st2)
instance Applicative CGMonad where
	pure a = CG \st.(Just a,st)
	(<*>) cgf cga = cgAp cgf cga
instance Functor CGMonad where
	fmap f cga = cgAp (return f) cga
	
cgOptional :: Bool (CGMonad ()) -> (CGMonad ())
cgOptional True a = a
cgOptional False _ = return ()
	
uptoCodeGeneration ::
	String
	([Error] -> Maybe a)
	([Error] -> a)
	([Error] -> Maybe a)
	([Error] -> a)
	([Error] -> a)
	([Error] -> a)
		-> Either a (String, a)
uptoCodeGeneration prog fscanErrors fparseFail fparseErrors fbindingErrors ftypeErrors fcodeErrors  =
	case uptoTypeInference prog fscanErrors fparseFail fparseErrors fbindingErrors ftypeErrors of
		Left a	= Left a
		Right (ast, typeErrors) =
			let (instructions, errors) = runCodeGenerator ast
			in Right (runCodeLabeler instructions, fcodeErrors (typeErrors ++ errors))/*case result of
				Right errors = Left $ fcodeErrors (typeErrors ++ errors)
				Left instructions = Right (runCodeLabeler instructions, typeErrors)*/

runCodeLabeler :: [CGInst] -> String
runCodeLabeler [] = "NONE";
runCodeLabeler instrs = foldl (+++)  "" (map labelInst instrs) 
	where
		labelInst :: CGInst -> String
		labelInst (Inst instr args label) =
			let
				prefix = case label of
							(Just l) = l +++ ": "
							(Nothing) = ""; 
				processedArgs = foldl (+++) "" (map (resolveArgument) args)
			
			in	prefix +++ instr +++ processedArgs +++ "\n"
		resolveArgument :: CGArg -> String
		resolveArgument (Val num) = " " +++ toString num
		resolveArgument (Annote str) = " " +++ str
		resolveArgument (Label l) = " " +++ l
		resolveArgument (Register MP) = " MP"
		resolveArgument (Register SP) = " SP"
		resolveArgument (Register RR) = " RR"
		resolveArgument (Register PC) = " PC" 
		resolveArgument (Register HP) = " HP"
		resolveArgument (Register BP) = " R5"
	
runCodeGenerator :: AST -> ([CGInst], [Error])
runCodeGenerator ast = 
	let
		(CG generator)	= generateCode ast
		(result, state) = generator zero
	in	(state.instructions, state.errors) 
	
encodeType :: Type -> String
encodeType (BasicType IntType) = "I";
encodeType (BasicType CharType) = "C";
encodeType (BasicType BoolType) = "B";
encodeType (ArrayType type) = "L_" +++ (encodeType type);
encodeType (TupleType aType bType) = "T_" +++ (encodeType aType) +++ "_" +++ (encodeType bType);


error :: Error -> CGMonad ()
error e = CG \st. (Nothing, { st & errors = [e : st.errors]})

debug :: Error -> CGMonad ()
debug e = CG \st. (Just (), { st & errors = st.errors ++ [e]})

registerGlobalVariable :: String -> CGMonad ()
registerGlobalVariable name = 
	CG \st. (Just (), {st & globalVariables = [name : st.globalVariables]})
		
registerLocalVariable :: String -> CGMonad ()
registerLocalVariable name = 
	CG \st. (Just (), {st & localVariables = [name : st.localVariables]})
		
registerInstructions :: [CGInst] -> CGMonad ()
registerInstructions instructions = CG \st. (
		Just (), 
		{st & instructions = st.instructions ++ instructions}
	)
	
setFunctionArguments :: [String] (CGMonad a) -> CGMonad a
setFunctionArguments arguments (CG nested) = CG \st.
	let
		previous = st.arguments
		(res, st2) = nested { st & arguments = arguments}
	in (res, {st2 & arguments = previous })
	
inFunction :: String (CGMonad a) -> CGMonad a
inFunction name (CG nested) = CG \st.
	let
		previous = st.currentFunction
		(res, st2) = nested {st & currentFunction = name}
	in (res, {st2 & currentFunction = previous})
	
getFunction :: (CGMonad String)
getFunction = CG \st.
	(Just st.currentFunction, st)
	
getArgumentCount :: (CGMonad Int)
getArgumentCount = CG \st.
	(Just (length st.arguments), st)
	
getNumGlobalVars :: (CGMonad Int)
getNumGlobalVars = CG \st.
	(Just (length st.globalVariables), st)
	
getNumLocalVars :: (CGMonad Int)
getNumLocalVars = CG \st.
	(Just (length st.localVariables), st)
	
getLocalVariables :: (CGMonad [String])
getLocalVariables = CG \st.
	(Just st.localVariables, st)
	
getGlobalVariables :: (CGMonad [String])
getGlobalVariables = CG \st.
	(Just st.globalVariables, st)
	
getArguments :: (CGMonad [String])
getArguments = CG \st.
	(Just st.arguments, st)
	
generateLabel :: CGMonad String
generateLabel = CG \st.
	(Just (st.currentFunction +++ "_l_" +++ (toString st.counter)), {st & counter = st.counter + 1})

clearLocals :: CGMonad ()
clearLocals = CG \st.
	(Just (), {st & localVariables = [], arguments = []})

validateKnownType :: Type -> CGMonad ()
validateKnownType (BasicType _) = return ()
validateKnownType (TupleType t1 t2) = validateKnownType t1 >>| validateKnownType t2
validateKnownType (ArrayType t) = validateKnownType t
validateKnownType (VoidType) = return ()
validateKnownType (IdentType _) = error {
							pos = zero,
							severity = FATAL,
							stage = CodeGeneration,
							message = "Cannot apply operation on unknown type"
						}

validateKnownTypeShallow :: Type -> CGMonad ()
validateKnownTypeShallow (BasicType _) = return ()
validateKnownTypeShallow (TupleType t1 t2) = return ()
validateKnownTypeShallow (ArrayType t) = return ()
validateKnownTypeShallow (VoidType) = return ()
validateKnownTypeShallow (IdentType _) = error {
							pos = zero,
							severity = FATAL,
							stage = CodeGeneration,
							message = "Cannot apply operation on unknown type"
						}
						
getType :: Expr -> (CGMonad Type)
getType expr = return (fromJust (getMeta expr).type)

/**
	Push code that will load the stack location of the variable on the stack.
	Look for the variable in order:
		1. Locals.
		2. Arguments.
		3. Globals.
**/
resolveAddress :: String -> CGMonad ()
resolveAddress id = 
		getNumLocalVars >>= \numLocalVars.
		getNumGlobalVars >>= \numGlobalVars.
		getArgumentCount >>= \numFuncVars.
		getArguments >>= \arguments.
		getGlobalVariables >>= \globalVariables.
		getLocalVariables >>= \localVariables.
		debug {
				pos = zero,
				severity = DEBUG,
				stage = CodeGeneration,
				message = ("get " +++ id +++ (gString{|*|} localVariables))
			} >>|
		case indexOf localVariables id of
			/**
				Note: due to clean list mechanics, the indexes are reversed.
					The first element will have the largest index.
			**/
			(Just index) = getStackVariable (numLocalVars - index)
			(Nothing) = case indexOf arguments id of
				(Just index) = getStackVariable (0 - index - 2)
				(Nothing) = case indexOf globalVariables id of
					(Just index) = getGlobalVariable (numGlobalVars - index)
					(Nothing) =  error {
							pos = zero,
							severity = FATAL,
							stage = CodeGeneration,
							message = "Variable " +++ id +++ " could not be found"
						}
	where
		getStackVariable :: Int -> CGMonad ()
		getStackVariable index = 
			registerInstructions [
				Inst "ldla" [Val (toString index)] (Just $ "_get_" +++ id)//Nothing
			]
		getGlobalVariable :: Int -> CGMonad ()
		getGlobalVariable index = 
			registerInstructions [
				Inst "ldc" [Label "_globals"] Nothing,
				Inst "ldc" [Val (toString (index + 17))] Nothing, // 17 = magic value
				Inst "add" [] Nothing
				//Inst "ldc" [Global index] Nothing,
				//Inst "lda" [Val "0"] Nothing
			]

indexOf :: [a] a -> Maybe Int | Eq a
indexOf list item = indexOfIter list item 0 
where 
	indexOfIter :: [a] a Int -> Maybe Int | Eq a
	indexOfIter [] _ _ = Nothing
	indexOfIter [a:b] item count
	| a == item = Just count
	| otherwise = indexOfIter b item (count + 1)
	
/**
	Push code that will load the address of the fielded variable on the stack.
**/
resolveFieldedAddress :: IdWithFields -> CGMonad ()
resolveFieldedAddress (JustId id metadata) = 
		resolveAddress id
	/*>>| registerInstructions [
				Inst "lda" [Val "0"] Nothing
			]*/
resolveFieldedAddress (WithField nest field	metadata) = 
	let
		// Load the heap valued pointed to - 1 on the stack
		loadNext = registerInstructions [
				Inst "lda" [Val "-1"] Nothing
			]
		
		// Substract 1 from the pointer
		movePointer = registerInstructions [
				Inst "ldc" [Val "1"] Nothing,
				Inst "sub" [] Nothing
			]
		
		// Get value of current address on stack
		retrieve = registerInstructions [
				Inst "lda" [Val "0"] Nothing
			]
	
		resolveField = case field of
			(FieldHd) 	= return ()
			(FieldFst) 	= return ()
			(FieldTl) 	= movePointer
			(FieldSnd) 	= movePointer
			 
	in 		resolveFieldedAddress nest
		>>| retrieve
		>>|	resolveField

generatePrint :: Type -> (CGMonad ())
generatePrint (BasicType bType) = 
		case bType of
			(CharType) = registerInstructions [Inst "trap" [Val "1"] Nothing]	
			(_) = registerInstructions [Inst "trap" [Val "0"] Nothing]
generatePrint (TupleType aType bType) =
			registerInstructions [
				Inst "lds" [Val "0"] Nothing,
				Inst "lda" [Val "0"] Nothing
			]
		>>|	generatePrint aType
		>>| registerInstructions [
				Inst "lds" [Val "0"] Nothing,
				Inst "lda" [Val "-1"] Nothing
			]
		>>|	generatePrint bType
		>>| registerInstructions [
				Inst "ajs" [Val "-1"] Nothing
			]
generatePrint (ArrayType type) =
			generateLabel >>= \startLabel.
			generateLabel >>= \endLabel.
			registerInstructions [
				Inst "lds" [Val "0"] (Just startLabel),
				Inst "lda" [Val "-1"] Nothing,
				Inst "brf" [Label endLabel] Nothing,
				Inst "lds" [Val "0"] Nothing,
				Inst "lda" [Val "0"] Nothing
			]
		>>|	generatePrint type
		>>| registerInstructions [
				Inst "lda" [Val "-1"] Nothing,
				Inst "bra" [Label startLabel] Nothing,
				Inst "ajs" [Val "-1"] (Just endLabel)
			]

generateRead :: Type -> (CGMonad ())
generateRead (BasicType IntType) = registerInstructions [
					Inst "trap" [Val "10"] Nothing
				]
generateRead (BasicType BoolType) = registerInstructions [
					Inst "trap" [Val "10"] Nothing
				]
generateRead (BasicType CharType) = registerInstructions [
					Inst "trap" [Val "11"] Nothing
				]
generateRead _ = error {
							pos = zero,
							severity = FATAL,
							stage = CodeGeneration,
							message = "Cannot read non-basic types"
						}
						
allocate :: (CGMonad ()) -> (CGMonad ())
allocate expr = generateLabel >>= \boundLabel.
				generateLabel >>= \endLabel.
				generateLabel >>= \expLabel.
				generateLabel >>= \retBoundLabel.
				generateLabel >>= \retUnBoundLabel.
				registerInstructions [ 
					Inst "ldr" [Register HP] Nothing,
					Inst "ldr" [Register BP] Nothing,
					Inst "eq" [] Nothing,
					
					// HP != BP, allocate and move BP to next empty spot
					Inst "brt" [Label boundLabel] Nothing,
					
					// Load data
					Inst "ldr" [Register BP] Nothing,
					Inst "ldh" [Val "0"] Nothing, // Load next address
					Inst "ldc" [Label retUnBoundLabel] Nothing,
					Inst "bra" [Label expLabel] Nothing,
					
					Inst "ldr" [Register BP] (Just retUnBoundLabel),
					Inst "stma" [Val "0", Val "3"] Nothing,
					Inst "ajs" [Val "-1"] Nothing,
					Inst "ldr" [Register BP] Nothing,
					Inst "swp" [] Nothing,
					Inst "str" [Register BP] Nothing,
					Inst "bra" [Label endLabel] Nothing,
					
					// HP == BP, allocate and copy HP to BP
					Inst "nop" [] (Just boundLabel),
					
					// Load data
					Inst "ldc" [Label retBoundLabel] Nothing,
					Inst "bra" [Label expLabel] Nothing,
					
					Inst "stmh" [Val "3"] (Just retBoundLabel),
					Inst "ldr" [Register HP] Nothing,
					Inst "str" [Register BP] Nothing,
					Inst "sts" [Val "-1"] Nothing,
					Inst "bra" [Label endLabel] Nothing
				]
			>>| registerInstructions [	
					// Store expression in one place
					Inst "nop" [] (Just expLabel),
					Inst "ldc" [Val "0"] Nothing
				]
			>>| expr
			>>| registerInstructions [
					Inst "lds" [Val "-3"] Nothing,
					Inst "str" [Register PC] Nothing,	
					// Common return
					Inst "nop" [] (Just endLabel)
				]
				
free :: Type -> (CGMonad ())
free (TupleType aType bType) = 
				registerInstructions [
					// Copy reference and load block on stack
					Inst "lds" [Val "0"] Nothing,
					Inst "ldmh" [Val "0", Val "3"] Nothing			
				]
				// Free both values
			//>>| free aType
			//>>| free bType
			>>| generateLabel >>= \endLabel.
				generateLabel >>= \ignoreLabel.
				// TODO: this can be factored out
				registerInstructions [
					// Decrease counter
					Inst "lds" [Val "-2"] Nothing,
					Inst "ldc" [Val "1"] Nothing,
					Inst "sub" [] Nothing,
					
					// Duplicate result
					Inst "lds" [Val "0"] Nothing,
					
					// Store new counter on the stack
					//Inst "lds" [Val "-4"] Nothing,
					//Inst "swp" [] Nothing,
					Inst "sts" [Val "-4"] Nothing,
					
					// If c == 0, push this empty spot
					Inst "ldc" [Val "0"] Nothing,
					Inst "le" [] Nothing,
					Inst "brf" [Label ignoreLabel] Nothing
				]
			>>|	free aType
			>>| free bType
			>>| registerInstructions [
					Inst "ajs" [Val "-1"] Nothing,
					Inst "ldc" [Val "2"] Nothing,
					Inst "sub" [] Nothing,
					
					// Swap values with BP, and store reference on stack
					Inst "lds" [Val "0"] Nothing,
					Inst "swpr" [Register BP] Nothing,
					Inst "swp" [] Nothing,
					Inst "sta" [Val "0"] Nothing,
					
					Inst "bra" [Label endLabel] Nothing,
					
					Inst "ajs" [Val "-2"] (Just ignoreLabel),
					Inst "swp" [] Nothing,
					Inst "sta" [Val "-2"] Nothing,
					
					// Common return
					Inst "nop" [] (Just endLabel)
				]
free (ArrayType type) = 
				generateLabel >>= \loopLabel.
				registerInstructions [
					// Copy reference and load block on stack
					Inst "lds" [Val "0"] (Just loopLabel),
					Inst "ldmh" [Val "0", Val "3"] Nothing			
				]
				// Free both values
			//>>| free type
			>>| generateLabel >>= \endLabel.
				generateLabel >>= \ignoreLabel.
				// TODO: this can be factored out
				registerInstructions [
					// Decrease counter
					Inst "lds" [Val "-2"] Nothing,
					Inst "ldc" [Val "1"] Nothing,
					Inst "sub" [] Nothing,
					
					// Duplicate result
					Inst "lds" [Val "0"] Nothing,
					
					// Store new counter on the stack
					Inst "sts" [Val "-4"] Nothing,
					
					// If c == 0, push this empty spot
					Inst "ldc" [Val "0"] Nothing,
					Inst "le" [] Nothing,
					Inst "brf" [Label ignoreLabel] Nothing
				]
			>>|	free type
			>>| registerInstructions [
					//Inst "lds" [Val "-2"] Nothing,
					
					// Store BP in this frame and update BP
					Inst "lds" [Val "-2"] Nothing,
					Inst "ldc" [Val "2"] Nothing,
					Inst "sub" [] Nothing,
					Inst "lds" [Val "0"] Nothing,
					Inst "ldr" [Register BP] Nothing,
					Inst "swp" [] Nothing,
					Inst "sta" [Val "0"] Nothing, // Store BP on heap
					Inst "str" [Register BP] Nothing, // Store new address in BP
					
					
					// Set reference to next list element
					Inst "sts" [Val "-2"] Nothing,
					Inst "ajs" [Val "-1"] Nothing,
					
					// Free the next element if there is one
					Inst "lds" [Val "0"] Nothing,
					Inst "brt" [Label loopLabel] Nothing,
					Inst "ajs" [Val "-1"] Nothing,
					Inst "bra" [Label endLabel] Nothing,
					
					// Update counter on heap
					Inst "ajs" [Val "-2"] (Just ignoreLabel),
					Inst "swp" [] Nothing,
					Inst "sta" [Val "-2"] Nothing,
					
					// Common return
					Inst "nop" [] (Just endLabel)
					//Inst "ajs" [Val "-1"] (Just endLabel)
				]
// We cannot free unknown or basic types
free (_) = registerInstructions [
					Inst "ajs" [Val "-1"] Nothing
				]

use :: Type -> (CGMonad ())
use type = case type of 
			(ArrayType type) 		= incrementCounter
			(TupleType aType bType) = incrementCounter
			_						= return ()
		where
			incrementCounter :: CGMonad ()
			incrementCounter = registerInstructions [
					// Load address of counter
					Inst "lds" [Val "0"] Nothing,
					
					// Load counter
					Inst "lds" [Val "0"] Nothing,
					Inst "lda" [Val "-2"] Nothing,
					Inst "ldc" [Val "1"] Nothing,
					Inst "add" [] Nothing,
					Inst "swp" [] Nothing,
					Inst "sta" [Val "-2"] Nothing
				]

/**
	Code generator implementation
**/
	
class generateCode 		a :: a  -> CGMonad ()
class generateCodes 	a :: [a] -> CGMonad ()
class generateCodeUn 	a :: a Type -> CGMonad ()
class generateCodeBin 	a :: a Type Type -> CGMonad ()

instance generateCode AST
where 
	generateCode ast =
		/**
			1. Reorder AST and put all global variables first.
			2. Generate code to initialize global variables, and note their position on the stack.
			3. Generate a bogus function call into the main() function.
			4. Generate code for the rest of the functions.
		**/
		let
			bootstrap = registerInstructions [
					Inst "ldr" [Register HP] Nothing,
					Inst "str" [Register BP] Nothing
				]	
		
			variables = filter isVarDecl ast
			functions = filter (not o isVarDecl) ast
			
			// Monad performing all variable initialisations.
			variableCode = sequence_ (
					map (\(Var dec=:(VarDecl _ name _ _)). generateCode dec >>| registerGlobalVariable name) (reverse variables)
				)
			
			// Monad creating all function code.
			functionCode = sequence_ (
					map (\(Fun dec). (clearLocals >>| generateCode dec)) (reverse functions)
				)
			
			// Monad to create the starting function call.
			mainCall = generateCode (FunCall "main" [] zero)
			
			// Create an instruction to end the program to prevent starting random functions.
			programEnd = registerInstructions [
					Inst "halt" [] Nothing
				]
				
			globalPointer = registerInstructions [
					Inst "halt" [] (Just "_globals")
				]
			
		in		bootstrap
			>>|	variableCode 	
			>>|	mainCall
			>>|	programEnd
			>>|	functionCode
			>>| globalPointer
		where
			isVarDecl :: Decl -> Bool
			isVarDecl (Var _) = True
			isVarDecl (Fun _) = False
		
		
instance generateCode Decl
where 
	generateCode decl =
		/**
			1. Passthrough code generation to specific method
		**/
		generateCode decl
		

instance generateCode VarDecl
where 
	generateCode (VarDecl mType id expr metadata) = 
		/**
			1. Generate code for expression.
			2. Leave reference on stack.
			
			- We do not register the variable here, as we do not know in what context
				we are working.
		**/
		let
			// Monad to create expression code
			type = fromJust (getMeta expr).type
			
			exprCode = generateCode expr 
				>>| use type 
		in exprCode
			

instance generateCode FunDecl
where 
	generateCode (FunDecl name args mType decls stmts metadata) =
		/**
			Stack layout on function call:
				x - 2 - n : return value
				x - 1 - n : argument 1
				...
				x - 1 - 1 : argument n
				x - 1	  : previous MP 
				x		  : return address (MP will point to this address)
				x + 1	  : local variable 1
				...
				x + n - 1 : local variable n-1
				x + n	  : local variable n
		
			1. Take note of all arguments.
			2. Generate code for all local variables, in normal order.
			3. Generate code for all statements.
				a. Return value will be placed in RR.
				b. Returns must jump to step 4, because we have to clean up.
					(We will need some kind of labeling with a second pass)
			4. Push the current stack value to the return position.
			5. Set SP to MP.
			6. Set RR to previous address (on SP, with str).
			7. Set MP to previous value (now on SP, with str).
			8. Set PC to RR, this will jump back to the parent function. 
				(With ldrr)
				
			It is up to the caller to clean the arguments.
				
		**/
		let
			// Monad performing all variable initialisations.
			variableMonads = map (
					\dec=:(VarDecl _ name _ _). 
					generateCode dec 
					>>| registerLocalVariable name
				) decls
				
			variableCode = sequence_ (
					variableMonads
				)
			
			// Monad creating all function code.
			stmtCode = sequence_ (
					map generateCode stmts
				)
				
			// Free local variables
			localTypes = map (
					\(VarDecl _ _ expr _). 
						fromJust (getMeta expr).type
				) decls
				
			freeLocals = sequence_ (
					map (free) (reverse localTypes)
				)
			
			// Free all arguments in reverse order
			/*(TS _ fType) = fromJust metadata.typeScheme
			(FuncType argTypes _) = fType
			
			freeCode = sequence_ (
					map (free) (reverse argTypes)
				)*/
			
			// Monad to clean up afterwards
			cleanUp = registerInstructions [
						// Set SP to MP + local variable count
						Inst "ldr" [Register MP] (Just (name +++ "_end")),
						Inst "ldc" [Val (toString (length decls))] Nothing,
						Inst "add" [] Nothing,
						Inst "str" [Register SP] Nothing
					]
				>>| freeLocals
				>>| registerInstructions [
						// Load the address into RR
						Inst "str" [Register RR] Nothing,
						// Load the previous MP into MP, to restore the previous context
						Inst "str" [Register MP] Nothing,
						// Jump back to caller
						Inst "ldrr" [Register PC, Register RR] Nothing
				]
			
		// Set function arguments so the variables are known	
		in	setFunctionArguments args (
					registerInstructions [
						Inst "nop" [] (Just (name +++ "_entry"))
					]
				>>| variableCode
				>>| inFunction name (
						stmtCode
					)
				//>>| freeCode
				>>| cleanUp
			)			
		where
			isVarDecl :: Decl -> Bool
			isVarDecl (Var _) = True
			isVarDecl (Fun _) = False
			
		
instance generateCodes Stmt
where 
	generateCodes stmts = 
		/**
			1. Generate code for every statement
		**/
		sequence_ (
			map generateCode stmts
		)
		
instance generateCode Stmt
where 
	generateCode (StmtIf cond stmts_a mStmts_b metadata) =
		/**
			1. Generate code for condition
			2. Generate code for stmts_a, and optionally mStmts_b, and take note of their length
			3. Generate testing code of expression result.
				a. Do not jump if cond succeeds, and just run stmts_a, if mStmts_b is set, add a jump afterwards
				b. If the condition does not succeed, jump over stmts_a
		**/
		let
			elseCode = case mStmts_b of
				(Just stmts) 	= generateCodes stmts
				(Nothing)		= return ()
		in 	generateLabel >>= \ifLabel.
			generateLabel >>= \elseLabel.
					generateCode cond
					// Jump to after the iflabel if the condition is false
				>>| registerInstructions [
						// We can ignore these, as the brf will jump only on zero
						//Inst "ldc" [Val "1"] Nothing,
						//Inst "eq" [] Nothing,
						Inst "brf" [Label ifLabel] Nothing
					]
				>>| generateCodes stmts_a
					// After the if, jump to the end
					// Use a nop to indicate the start of the 
				>>| registerInstructions [
						Inst "bra" [Label elseLabel] Nothing,
						Inst "nop" [] (Just ifLabel)
					]
				>>| elseCode
					// Another nop to have somewhere to jump to
				>>| registerInstructions [
						Inst "nop" [] (Just elseLabel)
					]
		
		/**
			1. Generate code for condition
			2. Generate code for statements and take note of length
			3. Output code as follows:
				a. Condition, jumps to after c if condition fails.
				b. Statements.
				c. Jump to a.
		**/
	generateCode (StmtWhile cond stmts metadata) =
			generateLabel >>= \condLabel.
			generateLabel >>= \endLabel.
					registerInstructions [
						Inst "nop" [] (Just condLabel)
					]
				>>|	generateCode cond
					// Jump to the endlabel if the condition is false
				>>| registerInstructions [
						//Inst "ldc" [Val "1"] Nothing,
						//Inst "eq" [] Nothing,
						Inst "brf" [Label endLabel] Nothing
					]
				>>| generateCodes stmts
				>>| registerInstructions [
						// Always jump back to the condition test
						Inst "bra" [Label condLabel] Nothing,
						Inst "nop" [] (Just endLabel)
					]
			
					
		/**
			1. Generate code for the expression.
			2. Lookup the location on the stack for the variable.
			3. Move the result to the location on the stack.
			
			For a direct assignment, we just move the new value to the stack.
		**/
	generateCode (StmtAss (JustId id metadata_2) expr metadata) = 
				getType expr >>= \type.
				// Get value of expression
				generateCode expr
				// Use the data
			>>| use type
				// This will push the address of the variable on the stack
			>>| resolveAddress id
				// Free the original value of the LHS
			>>| registerInstructions [
					Inst "lds" [Val "0"] Nothing
				]
			>>| free type
				// Store the result in the variable
			>>| registerInstructions [
					Inst "sta" [Val "0"] Nothing
				]
			
		/**
			1. Generate code for the expression.
			2. Lookup the location on the stack for the variable.
			3. Find the address on the heap of the value we are going to write to.
			4. Move the result on the stack to the heap.
			
			For a 'fielded' assignment we have to determine the destination first.
			Because it is not possible to just alter the heap, so instead:
				- Store HP on stack
				- Set HP to the location we want to update
				- Push the new value to the heap
				- Restore HP
		**/
	generateCode (StmtAss withField=:(WithField id field metadata_2) expr metadata) = 
			//	registerInstructions [
			//		"ldr" [Val "HP"] Nothing
			//	]
			//>>| 
				generateCode expr
				// This will push the address of the variable on the stack
			>>| resolveFieldedAddress withField
				// Store result of expr on location
			>>| registerInstructions [
				Inst "sta" [Val "0"] Nothing
			]
		
		
		/**
			1. Generate code to resolve idWithFields, result with address on stack.
			2. Generate code for expression.
			3. Store result in resolved location.
			
			Just pass through here.
		**/
	generateCode (StmtFunCall funCall=:(FunCall _ _ metadata) _) = 
				generateCode funCall
			>>| free (fromJust metadata.type)
			
		/**
			1. Generate code for expression.
			2. Save result in MP - 2 - |arguments|
			3. Jump to function end.
				(We need some labeling to find the end of the function)
		**/
	generateCode (StmtRet expr metadata) = 
				getArgumentCount
			>>= \arguments. getFunction
			>>= \function. generateCode expr
			>>| registerInstructions [
					Inst "stl" [Val (toString (-2 - arguments))] Nothing,
					Inst "bra" [Label (function +++ "_end")] Nothing
				]
				
				
		/**
			1. Jump to function end.
				(Again, need some label)
		**/
	generateCode (StmtRetV metadata) = 
				getFunction
			>>= \function. registerInstructions [
					Inst "bra" [Label (function +++ "_end")] Nothing
				]

instance generateCode [Expr]
where 
	generateCode exprs = 
		/**
			1. Generate code for all expressions.
			2. Concatenate both.
				(Works, as expressions simply leave an item on the stack)
				
			This is only used to generate code for function arguments.
		**/
		sequence_ (
			map generateCode exprs
		)
		
instance generateCode Expr
where 
	generateCode (ExpIdent variable metadata) = 
		/**
			1. Resolve variable name and location.
				a. Globals are on the beginning on the stack
				b. Arguments are before the MP
				c. Locals are after the MP
			2. Push resulting location on the stack.
		**/
				resolveFieldedAddress variable
			>>| registerInstructions [
					Inst "lda" [Val "0"] Nothing
				]
				
				
		/**
			1. Generate code for expr_a.
			2. Generate code for expr_b.
			3. Select the correct operator instructions for op.
				a. The operators know their overloading, so this can be delegated.
						
			This should probably do most of the work before the operator,
			as the operator itself is not really aware of the types.
		**/
	generateCode (ExpBinOp expr_a op expr_b metadata) = 
			let
				aType = fromJust (getMeta expr_a).type
				bType = fromJust (getMeta expr_b).type
			in	generateCode expr_a
			>>| generateCode expr_b
			>>| generateCodeBin op aType bType 
		
		/**
			1. Generate code for expr.
			2. Select the correct operator instructions for op.
				a. The operators know their overloading, so this can be delegated.
						
			This should probably do most of the work before the operator,
			as the operator itself is not really aware of the types.
		**/
	generateCode (ExpUnOp op expr metadata) = 
			let
				type = fromJust (getMeta expr).type
			in	generateCode expr
			>>| generateCodeUn op type 
		
		/**
			1. Push a literal int to the stack.
		**/
	generateCode (ExpInt int metadata) = 
			registerInstructions [
				Inst "ldc" [Val (toString int)] Nothing
			]
			
		/**
			1. Push a literal char to the stack.
		**/
	generateCode (ExpChar char metadata) = 
			registerInstructions [
				Inst "ldc" [Val (toString (toInt char))] Nothing
			]
			
		
		/**
			1. Push a literal bool to the stack.
		**/
	generateCode (ExpBool bool metadata) =
		let
			value = if bool (4294967295) 0
		in	registerInstructions [
				Inst "ldc" [Val (toString value)] Nothing
			]
		
		/**
			1. Passthrough generation for FunCall.
		**/
	generateCode (ExpFunCall funCall metadata) = 
				generateCode funCall
		/**
			1. Create an empty list item on the stack.
			2. Store address in stack frame.
		**/
	generateCode (ExpEmptyArray metadata) =
			allocate (
				registerInstructions [
					// Register two empty values.
					Inst "ldc" [Val "0"] Nothing,
					Inst "ldc" [Val "0"] Nothing
					// Push both values to the stack.
					//Inst "stmh" [Val "2"] Nothing
				]
			)			
			
		/**
			1. Generate code for expr_a and store result in heap.
			2. Generate code for expr_b and store result in heap.
			3. Store both references in heap, and store address in stack.
		**/
	generateCode (ExpTuple expr_a expr_b metadata) =
			// Reverse order! b is on x-1 and a on x!
			allocate (
				generateCode expr_b >>|
				generateCode expr_a >>|
				registerInstructions [
					// Push both items to the heap.
					//Inst "stmh" [Val "2"] Nothing
				]
			)
		
instance generateCode FunCall
where generateCode (FunCall id exprs metadata) =
		/**
			1. Resolve address of id.
			2. Allocate return value.
			3. Push arguments on stack.
			4. Push current MP on stack.
			5. Push PC on stack.
			6. Jump to function.
		**/
			let
				loadArguments = sequence_ (
						map (\expr. generateCode expr >>| getType expr >>= use) (exprs)
					)
				
				freeArguments = sequence_ (
						map (\expr. getType expr >>= free) (exprs)
					)
			in  generateLabel >>= \returnLabel. 	
				registerInstructions [
						Inst "ldc" [Val "0"] Nothing // Return value
					]
					// Push arguments
				>>|	loadArguments
				>>| cgOptional (id == "isEmpty") (
						registerInstructions [
							Inst "lda" [Val "0"] Nothing,
							Inst "ldc" [Val "0"] Nothing,
							Inst "eq" [] Nothing,
							Inst "sta" [Val "-1"] Nothing
						]
					) 
				>>| cgOptional (id == "read") (
						generateRead (fromJust metadata.type)
					) 
				>>| cgOptional (id == "print") (
						return () >>= \_.
							let
								head = hd exprs
							in 	getType head 
							>>= \type.generatePrint (type)
					) 
				>>| cgOptional (id <> "isEmpty" && id <> "read" && id <> "print") (
						registerInstructions [
							// Push old MP to stack
							Inst "ldr" [Register MP] Nothing,
							// Set MP, take SP and add 1 (so it points to the return address)
							Inst "ldr" [Register SP] Nothing,
							Inst "ldc" [Val "1"] Nothing,
							Inst "add" [] Nothing,
							Inst "str" [Register MP] Nothing ,
							// Branch to subroutine, pushes PC+1 to stack, and jumps
							Inst "bsr" [Label (id +++ "_entry")] Nothing
							// Clean arguments from stack
							//Inst "ajs" [Val (toString (~(length exprs)))] Nothing
						]
					)
				>>| freeArguments
			


instance generateCodeBin BinOp
where generateCodeBin operator aType bType =
		/**
			Operators:
			a. Operators only function on known types. An error is thrown if an operator
				is called on a non-basic type.
			b. Ints/Chars/Bools are manipulated on directly on the stack.
			c. Concatenation of lists is allowed even though the list type is unknown.
			d. Behaviour:
				- Concat takes either a value or an address on the LHS, and an address on the RHS.
					The memory layout requires that the new item on the heap has the reference to
					the nest element first, and then the value. So the top two items on the stack
					have to be swapped.
				- Equality gets a tailor-made equivalence test for the given type.
					Note: input has to be consumed.
				- NE just inverts the result of equality.
			
			1. Check if LHS and RHS are of a basic type.
			2. Generate code for both sides.
			3. Generate code for operator.
		**/
		case operator of
			// TODO:
			// Concat needs to shuffle its arguments around to work with the counting
			(OpConcat) 	= 
							validateKnownTypeShallow bType
					  	>>| use bType
					  	>>| allocate (
						  	registerInstructions [
								Inst "ldc" [Val "0"] Nothing,
								Inst "ldc" [Val "0"] Nothing
							]
						)
					  	>>| registerInstructions [
					  			Inst "lds" [Val "-2"] Nothing,
					  			Inst "lds" [Val "-1"] Nothing,
					  			Inst "sta" [Val "0"] Nothing,
					  			Inst "ldms" [Val "-1", Val "2"] Nothing,
					  			Inst "sta" [Val "-1"] Nothing,
					  			Inst "sts" [Val "-2"] Nothing,
					  			Inst "ajs" [Val "-1"] Nothing
					  		]
			(OpEquals) 	= validateKnownType aType
					  >>| validateKnownType bType
					  >>| generateEquivalence aType
			(OpNE)		= generateEquivalence aType
					  >>| generateCodeUn OpNot aType
			(genOp)		= // All generic operators that just use a different opcode.
				let
					inst = case operator of
						(OpPlus) 	= "add"
						(OpMinus)	= "sub"
						(OpMult) 	= "mul"
						(OpDiv)  	= "div"
						(OpMod)		= "mod"
						(OpLT)		= "lt"
						(OpGT)		= "gt"
						(OpLTE)		= "le"
						(OpGTE)		= "ge"
						(OpAnd)		= "and"
						(OpOr)		= "or"
				in 	registerInstructions [
						Inst inst [] Nothing
					]
		where
			generateEquivalence :: Type -> CGMonad ()
			generateEquivalence (BasicType _) =
				registerInstructions [
					Inst "eq" [] Nothing
				]
			generateEquivalence type=:(ArrayType nType) =
					generateLabel >>= \endLabel.
					generateLabel >>= \nonEquivLabel.
					generateLabel >>= \nonEmptyLabel.
					generateLabel >>= \startLabel.
					registerInstructions [
						// Load result value on stack
						Inst "ldc" [Val "-1"] Nothing,
						
						// This is equivalent to a.isEmpty == b.isEmpty
				
						// Load reference of LHS on stack, and check if this is the empty list
						Inst "lds" [Val "-2"] (Just startLabel),
						Inst "ldh" [Val "-1"] Nothing,
						Inst "ldc" [Val "0"] Nothing,
						Inst "eq" [] Nothing,
						
						// Load reference of RHS on stack
						Inst "lds" [Val "-2"] Nothing,
						Inst "ldh" [Val "-1"] Nothing,
						Inst "ldc" [Val "0"] Nothing,
						Inst "eq" [] Nothing,
						
						Inst "eq" [] Nothing,
						
						// Jump to end and set result to 0 on mismatch
						Inst "brt" [Label nonEmptyLabel] Nothing,
						Inst "ldc" [Val "0"] Nothing,
						Inst "and" [] Nothing,
						Inst "bra" [Label endLabel] Nothing
					]
				>>|	registerInstructions [
						// Test if both of the lists are the empty list
					
						// Test if LHS is the empty list
						Inst "lds" [Val "-2"] (Just nonEmptyLabel),
						Inst "ldh" [Val "-1"] Nothing,
						Inst "ldc" [Val "0"] Nothing,
						Inst "eq" [] Nothing,
						// Test if RHS is the empty list
						Inst "lds" [Val "-2"] Nothing,
						Inst "ldh" [Val "-1"] Nothing,
						Inst "ldc" [Val "0"] Nothing,
						Inst "eq" [] Nothing,
						
						// Test if both lists are empty
						Inst "and" [] Nothing,
						Inst "brt" [Label endLabel] Nothing,
				
						// Here we can assume that both lists have an item
						// Because we checked that a.isEmpty == b.isEmpty
						// And they are not both empty
				
						// Perform value equivalence check
				
						// Load value of LHS on stack
						Inst "lds" [Val "-2"] (Just startLabel),
						Inst "ldh" [Val "0"] Nothing,
						// Load value of RHS on stack
						Inst "lds" [Val "-2"] Nothing,
						Inst "ldh" [Val "0"] Nothing
					]
				>>| generateEquivalence nType
				>>|	registerInstructions [
						// Store result in stack
						Inst "and" [] Nothing
					]
				>>|	registerInstructions [
						// Test if one of the current lists are the empty list
						// If so, return nonequal
						// If not, update references and restart loop
						
						// Load reference of LHS on stack, and check if this is the empty list
						Inst "lds" [Val "-2"] Nothing,
						Inst "ldh" [Val "-1"] Nothing,
						Inst "sts" [Val "-3"] Nothing,
						
						// Load reference of RHS on stack
						Inst "lds" [Val "-1"] Nothing,
						Inst "ldh" [Val "-1"] Nothing,
						Inst "sts" [Val "-2"] Nothing,
						
						// Jump to top of loop
						Inst "bra" [Label startLabel] Nothing,
						
						// We reached the end
						Inst "sts" [Val "-2"] (Just endLabel),
						Inst "ajs" [Val "-1"] Nothing
					]
			generateEquivalence (TupleType aType bType) =
					// Compare left side
					registerInstructions [
						// Load value of LHS on stack
						Inst "lds" [Val "-1"] Nothing,
						Inst "ldh" [Val "0"] Nothing,
						// Load value of RHS on stack
						Inst "lds" [Val "-1"] Nothing,
						Inst "ldh" [Val "0"] Nothing
					]
				>>| generateEquivalence aType
				>>| // Compare right side
					registerInstructions [
						// Load value of LHS on stack
						Inst "lds" [Val "-2"] Nothing,
						Inst "ldh" [Val "-1"] Nothing,
						// Load value of RHS on stack
						Inst "lds" [Val "-2"] Nothing,
						Inst "ldh" [Val "-1"] Nothing
					]
				>>| generateEquivalence bType
				>>| // Move result to top
					registerInstructions [
						Inst "and" [] Nothing,
						Inst "sts" [Val "-2"] Nothing,
						Inst "ajs" [Val "-1"] Nothing,
						Inst "str" [Register SP] Nothing
					]
		
instance generateCodeUn UnOp
where generateCodeUn operator type =
		/**
			1. Check if the argument is of a known type.
			2. Generate code for the operator.
		**/
		let
			inst = case operator of
				(OpNot) = "not"
				(OpNeg) = "neg"
		in 	registerInstructions [
				Inst inst [] Nothing
			]
			