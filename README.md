# funcprog-m3nu
Project Functioneel Programmeren - Parser

lowercase alles

if/else = hunry/stuffed {}
while = eating {}
true/false = tasty/disguisting
x = 5    -> order x 5
bool exp = and/or ipv && / ||
arith exp = + - * / %
comments = review test
print = puke "test"
sleep = cook (want kan niet eten tijdens koken) tijd in seconden!

semicolons verplicht
newlines verplicht
expressies moeten tussen ()

sources
https://wiki.haskell.org/Parsing_a_simple_imperative_language
slides - Monads
http://dev.stephendiehl.com/fun/WYAH.pdf
https://cseweb.ucsd.edu/classes/wi12/cse230-a/lectures/parsers.html
http://stackoverflow.com/questions/16970431/implementing-a-language-interpreter-in-haskell
parsec lib voor hulpfuncties
https://en.wikibooks.org/wiki/Haskell/ParseExps
https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/12-State-Monad

features
- bools worden als doubles gezien zoals in C en PHP ,zelfde als python (0 = false, rest is true)
- nummers zijn doubles want ints zijn doubles maar doubles zijn geen ints
- vrij modulair
- whitespace maakt totaal niet uit (moet wel minstens 1 whitespace na identifier staan)
- robot deel staat los van volledige taal
