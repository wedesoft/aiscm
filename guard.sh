#!/bin/sh
while true; do
  inotifywait -e CLOSE_WRITE `git ls-files`;
  clear;
  cd tests && rm -f $@ && make $@ && cd ..
done
