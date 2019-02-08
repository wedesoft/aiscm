#!/bin/sh
while true; do
  clear;
  rm -f *.log;
  make test_$@.log;
  inotifywait -e CLOSE_WRITE `git ls-files ..`;
done
