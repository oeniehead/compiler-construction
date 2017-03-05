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
- ExpNested verwijderen?a

# Log
02-03 11.30-13.00 Tom CL interfacing en comments negeren