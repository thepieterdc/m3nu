# funcprog-m3nu
Project Functioneel Programmeren - Parser

if/else = hunry/stuffed
fi = satisfied
while = eating | enough
true/false = tasty/disguisting
x = 5    -> order x 5
bool exp = and/or ipv && / ||
comments = review test
print = puke "test"

semicolons mogen maar zijn niet veprlicht

sources
https://wiki.haskell.org/Parsing_a_simple_imperative_language
slides - Monads
http://dev.stephendiehl.com/fun/WYAH.pdf
https://cseweb.ucsd.edu/classes/wi12/cse230-a/lectures/parsers.html
http://stackoverflow.com/questions/16970431/implementing-a-language-interpreter-in-haskell
parsec lib voor hulpfuncties

features
- semicolons niet veprlicht
- vrij modulair
- whitespace maakt totaal niet uit (moet wel minstens 1 whitespace na identifier staan)
- robot deel staat los van volledige taal
