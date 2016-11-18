#!/bin/sh
while true; do
  clear;
  make check -j 4 | grep FAIL;
  inotifywait -e CLOSE_WRITE `git ls-files`;
done
