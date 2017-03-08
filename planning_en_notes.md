# To do
Parsing testen
Pretty printing
Error messages parser?

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
	
# Issues
- De parser geeft bij een van de voorbeelden soms geen error, terwijl ie dat wel zou moeten doen. Ik weet niet meer welke.

# Notes/niet vergeten
- Warning of error als EOF voordat een multiline comment is geeindigd?
- Control en data worden uit iTasks SDK gehaald, verwijderen uit project
- parse EOFToken
- error voor als er meerdere/geen parse results zijn
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

# Vragen
- In a_bit_of_everything in examples: the if in the function abs has no braces. accidentally?
(- in bool forgot ::)

# Log
02-03 11.30-13.00 Tom CL interfacing en comments negeren