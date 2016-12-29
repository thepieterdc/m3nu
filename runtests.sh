#!/bin/sh
TESTS=courses/tests/*.course
for t in $TESTS
do
  echo -n "Testing: $t "
  runhaskell Main.hs $t > /dev/null
  echo ""
done
