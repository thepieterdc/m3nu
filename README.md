# funcprog-m3nu
Project Functioneel Programmeren - Parser

lowercase alles

if/else = hunry/stuffed {}
while = eating {}
true/false = tasty/disguisting
x = 5    -> order x 5
bool exp = and/or ipv && / ||
arith exp = + - * / % > < >= <= ==
comments = review test
print = puke "test"
sleep = cook (want kan niet eten tijdens koken) tijd in seconden!
led left 0 0 0 = led 1    led left off = uit, led left white blue red green yellow 
led right 0 0 0 = led 2
linesensor = positie in [BOTHW, LEFTB, RIGHTB, BOTHB]
not x = 0 als niet 0 anders 1
drive forward left right backward brake
drive backwardleft/right = rij achterwaarts op het andere wiel
  -> backwardright laat het rechtse wiel staan

robotstuff uiteraard niets te zien met eten want kan niet
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
- fromjust want gaat niet werken als waarde niet bestaat
- bools worden als doubles gezien zoals in C en PHP ,zelfde als python (0 = false, rest is true)
- nummers zijn doubles want ints zijn doubles maar doubles zijn geen ints
- vrij modulair
- whitespace maakt totaal niet uit (moet wel minstens 1 whitespace na identifier staan)
- overal hlint/gekeken om het te verkorten
