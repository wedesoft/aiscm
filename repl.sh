#!/bin/sh
LD_LIBRARY_PATH=$(dirname $0)/aiscm/.libs:$LD_LIBRARYPATH \
  guile --no-auto-compile -L $(dirname $0)
