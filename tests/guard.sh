#!/bin/sh
while true; do
  clear;
  rm -f *.log;
  make $@;
  inotifywait -e CLOSE_WRITE `git ls-files ..`;
done
