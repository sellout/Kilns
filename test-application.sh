#!/bin/sh

./build-application.sh
./kilns -c ${1:-2} < tests/run-tests.kiln
