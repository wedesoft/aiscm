#!/bin/sh
while true; do
  clear;
  rm -f *.log;
  make $@.log;
  inotifywait -e CLOSE_WRITE `git ls-files ..`;
done
