#!/bin/sh
while true; do
  inotifywait -e CLOSE_WRITE $@;
  clear;
  make
done
