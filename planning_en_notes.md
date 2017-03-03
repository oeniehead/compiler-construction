# To do
Parsing testen
Pretty printing

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
- readString is recursief gedefinieerd. Om de een of andere reden raakt hij in een oneindige recursie als je hem uitvoert. ik heb de indruk dat ie hem wilt uitschrijven, waarom weet ik ook niet.

# Notes/niet vergeten
- Warning of error als EOF voordat een multiline comment is geeindigd?
- Control en data worden uit iTasks SDK gehaald, verwijderen uit project

# Log
02-03 11.30-13.00 Tom CL interfacing en comments negeren