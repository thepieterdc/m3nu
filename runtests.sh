#!/bin/sh
TESTS=courses/tests/*.course
for t in $TESTS
do
  echo "Testing: $t"
  runhaskell Main.hs $t
done
