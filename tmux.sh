#!/bin/bash
tmux split-window -d -h
tmux split-window -d -p 70
tmux select-pane -t0
tmux send-keys "make repl -j 4" C-m
tmux select-pane -t1
tmux send-keys "cd tests" C-m
tmux send-keys "./guard.sh hypercomplex" C-m
tmux select-pane -t2
tmux send-keys "nvim" C-m
sleep 1
tmux send-keys C-O C-O
tmux select-pane -t1
