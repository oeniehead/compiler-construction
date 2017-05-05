implementation module CodeGenerator

import AST

import Data.Functor
import Control.Applicative
import Control.Monad

import qualified Data.Map as m
from Data.Map import instance Functor (Map k)

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
	| Next 	String	// To the position after this label

:: CGInst
	= Inst String [CGArg] (Maybe String)

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
					x - 1:	Pointer to first item.
					x:		Pointer to second item.
					
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
		(Nothing,st2)	= (Nothing st2)

instance Monad CGMonad where
	bind (M ma) f = M \st.
		case ma st of
			(Just a, st2)	= let (M mb) = f a in mb st2
			(Nothing,st2)	= (Nothing,st2)
instance Applicative CGMonad where
	pure a = M \st.(Just a,st)
	(<*>) mf ma = mAp mf ma
instance Functor CGMonad where
	fmap f cga = cgAp (return f) cga
	
error :: Error -> CGMonad Error
error e = CG \st. (Just e, { st & errors = [e : st.errors]}

registerGlobalVariable :: Decl -> CGMonad Decl
registerGlobalVariable dec = case dec of
	(Fun _ _) = error {
						pos = zero,
						severity = FATAL,
						stage = CG,
						message = "A function is not a variable"
					} >>| return dec
	(Var (VarDecl _ name _ _) _) =
		// Push name to the end of the list, because their index must be the highest.
		CG \st. (Just dec, {st & globalVariables = [name : st.globalVariables]})
		
registerLocalVariable :: Decl -> CGMonad Decl
registerLocalVariable dec = case dec of
	(Fun _ _) = error {
						pos = zero,
						severity = FATAL,
						stage = CG,
						message = "A function is not a variable"
					} >>| return dec
	(Var (VarDecl _ name _ _) _) =
		// Push name to the end of the list, because their index must be the highest.
		CG \st. (Just dec, {st & localVariables = [name : st.localVariables]})
		
registerInstructions :: [GCInst] -> CGMonad [GCInst]
registerInstructions instructions = GC \st. (
		Just instructions, 
		{st & instructions = instructions ++ st.instructions}
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
	
generateLabel :: CGMonad String
generateLabel = CG \st.
	(Just "label%" +++ (toString st.counter), {st & counter = st.counter + 1})
		
		
/**
	Code generator implementation
**/
	
class generateCode a :: a -> CGMonad a

instance generateCode AST
	where generateCode ast =
		/**
			1. Reorder AST and put all global variables first.
			2. Generate code to initialize global variables, and note their position on the stack.
			3. Generate a bogus function call into the main() function.
			4. Generate code for the rest of the functions.
		**/
		let
			variables = filter isVarDecl ast
			functions = filter (not o isVarDecl) ast
			
			// Monad performing all variable initialisations.
			variableCode = sequence_ (
				map (\var. (generateCode var) >>= registerGlobalVariable) variables
			)
			
			// Monad creating all function code.
			functionCode = sequence_ (
				map generateCode functions
			)
			
			// Monad to create the starting function call.
			mainCall = generateCode (FunCall "main" [] zero)
			
			// Create an instruction to end the program to prevent starting random functions.
			programEnd = registerInstructions [
				Inst "halt" [] "main%end"
			]
			
		in	variableCode 	>>|
			mainCall		>>|
			programEnd		>>|
			functionCode
		where
			isVarDecl :: Decl -> Bool
			isVarDecl (Var _ _) = True
			isVarDecl (Fun _ _) = False
		
		
instance generateCode Decl
	where generateCode decl metadata =
		/**
			1. Passthrough code generation to specific method
		**/
		generateCode decl
		

instance generateCode VarDecl
	where generateCode (VarDecl mType id expr metadata) = 
		/**
			1. Generate code for expression.
			2. Leave reference on stack.
			
			- We do not register the variable here, as we do not know in what context
				we are working.
		**/
		let
			// Monad to create expression code
			exprCode = generateCode expr
		in exprCode
			

instance generateCode FunDecl
	where generateCode (FunDecl name args mType decls stmts metadata =
		/**
			Stack layout on function call:
				x - n+2 : return value
				x - n+1	: argument n
				...
				x - 2	: argument 1
				x - 1	: previous MP 
				x		: return address (MP will point to this address)
				x + 1	: local variable 1
				...
				x + n-1 : local variable n-1
				x + n	: local variable n
		
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
			variableCode = sequence_ (
				map (\var. (generateCode var) >>= registerLocalVariable) decls
			)
			
			// Monad creating all function code.
			stmtCode = sequence_ (
				map generateCode stmts
			)
			
			// Monad to clean up afterwards
			cleanUp = registerInstructions [
				// Push return value to begin of frame
				Inst "stl" ["-" +++ (toString ((length arguments) + 1)] (Just  name +++ "%end"),
				// Move MP to SP, so we point to the return address
				Inst "ldrr" [Val "SP", Val "MP"] Nothing,
				// Load the address into RR
				Inst "str" [Val "RR"] Nothing,
				// Load the previous MP into MP, to restore the previous context
				Inst "str" [Val "MP"] Nothing
			]
			
		// Set function arguments so the variables are known	
		in	setFunctionArguments args (
				variableCode
			>>| inFunction name (
					stmtCode
				)
			>>| cleanUp
		)
			
		where
			isVarDecl :: Decl -> Bool
			isVarDecl (Var _ _) = True
			isVarDecl (Fun _ _) = False
			
		
instance generateCode [Stmt]
	where generateCode stmts = 
		/**
			1. Generate code for every statement
		**/
		sequence_ (
			map generateCode stmts
		)
		
instance generateCode Stmt
	where generateCode (StmtIf cond stmts_a mStmts_b metadata) =
		/**
			1. Generate code for condition
			2. Generate code for stmts_a, and optionally mStmts_b, and take note of their length
			3. Generate testing code of expression result.
				a. Do not jump if cond succeeds, and just run stmts_a, if mStmts_b is set, add a jump afterwards
				b. If the condition does not succeed, jump over stmts_a
		**/
		let
			
		in 	generateLabel >>= \ifLabel.
			generateLabel >>= \elseLabel.
					generateCode cond
				>>| registerInstructions [
						Inst "ldc" [Val "1"] Nothing,
						Inst "eq" [] Nothing,
						Inst "brf" [Next ifLabel] 
					]
			
		
		
		generateCode (StmtWhile cond stmts metadata) =
		/**
			1. Generate code for condition
			2. Generate code for statements and take note of length
			3. Output code as follows:
				a. Condition, jumps to after c if condition fails.
				b. Statements.
				c. Jump to a.
		**/
		generateCode (StmtAss idWithFields expr metadata) = 
		/**
			1. Generate code to resolve idWithFields, result with address on stack.
			2. Generate code for expression.
			3. Store result in resolved location.
		**/
		generateCode (StmtFunCall funcall metadata) = 
		/**
			1. Passthrough generation for FunCall
			
			RR value can be ignored here.
		**/
		generateCode (StmtRet expr metadata) = 
		/**
			1. Generate code for expression.
			2. Push result to RR.
			3. Jump to function end.
				(We need some labeling to find the end of the function)
		**/
		generateCode (StmtV expr metadata) = 
		/**
			1. Push zero to RR.
			2. Jump to function end.
				(Again, need some label)
		**/

instance generateCode [Expr]
	where generateCode exprs = 
		/**
			1. Generate code for all expressions.
			2. Concatenate both.
				(Works, as expressions simply leave an item on the stack)
		**/
		sequence_ (
			map generateCode exprs
		)
		
instance generateCode Expr
	where generateCode (ExpIdent IdWithFields metadata) = 
			/**
				1. Resolve variable name and location.
					a. Globals are on the beginning on the stack
					b. Arguments are before the MP
					c. Locals are after the MP
				2. Push resulting location on the stack.
			*/
		generateCode (ExpBinOp expr_a op expr_b metadata) = 
			/**
				1. Generate code for expr_a.
				2. Generate code for expr_b.
				3. Select the correct operator instructions for op.
					a. The operators know their overloading, so this can be delegated.
							
				This should probably do most of the work before the operator,
				as the operator itself is not really aware of the types.
			**/
		generateCode (ExpUnOp expr metadata) = 
			/**
				1. Generate code for expr.
				2. Select the correct operator instructions for op.
					a. The operators know their overloading, so this can be delegated.
							
				This should probably do most of the work before the operator,
				as the operator itself is not really aware of the types.
			**/
		generateCode (ExpInt int metadata) = 
			/**
				1. Push a literal int to the stack.
			**/
		generateCode (ExpChar char metadata) = 
			/**
				1. Push a literal char to the stack.
			**/
		generateCode (ExpBool bool metadata) =
			/**
				1. Push a literal bool to the stack.
			**/
		generateCode (ExpFunCall funCall metadata) = 
			/**
				1. Passthrough generation for FunCall
			**/
		generateCode (ExpEmptyArray metadata) =
			/**
				1. Create an empty list item on the stack.
				2. Store address in stack frame.
			**/
			registerInstructions [
				// Register two empty values.
				Inst "ldc" [Val "0"] Nothing,
				Inst "ldc" [Val "0"] Nothing,
				// Push both values to the stack.
				Inst "stmh" [Val "2"] Nothing,
			]
		generateCode (ExpTuple expr_a expr_b) =
			/**
				1. Generate code for expr_a and store result in heap.
				2. Generate code for expr_b and store result in heap.
				3. Store both references in heap, and store address in stack.
			**/
			generateCode expr_a >>|
			generateCode expr_b >>|
			registerInstructions [
				// Push both items to the heap.
				Inst "stmh" [Val "2"] Nothing,
			]
		
instance generateCode FunCall
	where generateCode (FunCall id exprs metadata) =
		/**
			1. Resove address of id
			2. Generate code for all exprs in reverse order.
				(This sets up all the arguments)
			3. Push PC + 4 to the stack
			4. Push current MM to the stack
			5. Set MM to top of stack
			6. Jump to resolved function address
		**/
		
instance generateCode BinOp
	where generateCode operator =
		/**
			Operators:
			a. Operators only function on known types. An error is thrown if an operator
				is called on a non-basic type.
			b. Ints/Chars/Bools are manipulated on directly on the stack.
			c. Concatenation of lists is allowed even though the list type is unknown.
			d. Behaviour:
				- Concat takes either a value or an address on the LHS, and an address on the RHS.
					A new item containing LHS is pushed to the heap, the item pointed to by RHS
					is updated to link to LHS. The address of this new item is left on the stack.
				- Equality does a simple value comparison for Int/Char/Bool. For lists and tuples 
					some kind of subroutine should be implemented. We can only test equality on
					known types.
			
			1. Check if LHS and RHS are of a basic type.
			2. Generate code for both sides.
			3. Generate code for operator.
		**/
		
instance generateCode UnOp
	where generateCode operator =
		/**
			1. Check if the argument is of a known type.
			2. Generate code for the operator.
		**/
		
instance generateCode IdWithFields
	where generateCode (WithField id field metadata) =
		/**
			1. Generate code for child id first.
			2. Take address on stack and select next address/value.
			2. Leave result on stack.
		**/
		generateCode (JustId id metadata) =
		/**
			1. Push content of variable to stack.
		**/
			