#!/bin/sh

./build-application.sh test-implementation
./kilns -c ${1:-2} tests/run-tests.kiln
