#!/bin/sh
while true; do
  clear;
  rm -f *.log;
  make $@;
  sleep 2;
  less $@;
  inotifywait -e CLOSE_WRITE `git ls-files ..`;
done
