#!/bin/sh

if [ -f './kilns' ]; then
   rm ./kilns
fi

./build-application.sh test-implementation

if [ -f './kilns' ]; then
   ./kilns -c ${1:-2} tests/run-tests.kiln
else
   echo '`kilns` does not exist'
fi


