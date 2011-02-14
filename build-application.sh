#!/bin/sh

dx86cl64 --load "build/build-application"

if [ $# -gt 0 ]; then
   # the QUICKLOAD is for quieter system loading
   dx86cl64 --eval '(load "kilns.asd")' \
            --eval '(ql:quickload "kilns")' \
            --eval '(asdf:test-system :kilns)'
            --eval '(quit)'
fi
