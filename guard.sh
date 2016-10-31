#!/bin/sh
while true; do
  clear;
  cd tests && rm -f $@ && make $@ && cd ..
  inotifywait -e CLOSE_WRITE `git ls-files`;
done
