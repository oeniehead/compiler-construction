# To do
file io schrijven -> Automatisch testen met voorbeeldprogramma's op bb
manier vinden om code te testen
Scanner moet comments negeren
Parsing
	- transform grammar
		- precedence
		- remove left-recursion
			1. Remove empty productions
			2. Eliminate cycles
			3. Eliminate direct left-recursion A ::= A.... | ...
	- The real parsing
Pretty printing

# Issues
- readString is recursief gedefinieerd. Om de een of andere reden raakt hij in een oneindige recursie als je hem uitvoert. ik heb de indruk dat ie hem wilt uitschrijven, waarom weet ik ook niet.

# Notes/niet vergeten
- Control en data worden uit iTasks SDK gehaald, verwijderen uit project