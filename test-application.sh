#!/bin/sh

./build-application.sh
./kilns < tests/run-tests.kiln
