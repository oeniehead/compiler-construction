# To do
Parsing testen
Scannen van escape sequence characters
Report schrijven

#Done
Scanner moet comments negeren
Parsing
	- transform grammar
		- precedence
		- remove left-recursion
			1. Remove empty productions
			2. Eliminate cycles
			3. Eliminate direct left-recursion A ::= A.... | ...
	- The real parsing
Scanning testen
Pretty printing

	
# Issues

# Notes/niet vergeten
- Tests:
	- parse == parse o prettyprint o parse
	- parse o prettyprint == parse o prettyprint o parse o prettyprint
	- vooral de stress.spl is een goede om te testen

# Vragen
- In a_bit_of_everything in examples: the if in the function abs has no braces. accidentally?
(- in bool forgot ::)

# Log(Begint vanaf 10-3-17)
02-03 11.30-13.00	Tom		CL interfacing en comments negeren


# Whishlist (could haves)
Parser:
- Error messages
	- meer error messages waar mogelijk
	- error voor als er meerdere/geen parse results zijn
	- Warning of error als EOF voordat een multiline comment is geeindigd?
	- Unscannable token?
	* Error messages in de parser combinators
	* parser combinator returnt
		* ofwel niets en de continuation (betekent dat dit niet de juiste regel was)
		* ofwel alle mogelijke parse trees en eventueel specifieke error messages en de continuation
- Recovery na unscannable tokens/errors
- expression parsing: see comments in SPLParser. We could also parse all possibilities
	of precedence of + -, :, == < and check if one is possible(or maybe try many different
	precedences for : in an expression, if you have only one precedence that gives a
	result, return that result, else give an error message)
Interesting cases:
1 < 2 : 3 > 4 : [] == 1 < 2 : 4 > 5 : []
[] == [] : []
[] == ([] : []) :: Bool
([] == []) : [] :: [Bool]
True || False : [] is parsed True || (False : []) i.s.o. (True || False) : []

PrettyPrinter:
- pretty printen van expressies