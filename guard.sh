#!/bin/sh
while true; do
  inotifywait -e CLOSE_WRITE `git ls-files`;
  clear;
  make check -j
done
