#!/bin/bash

# Check if a PID is provided as an argument
if [ -z "$1" ]; then
  echo "Usage: $0 <PID>"
  exit 1
fi

PID=$1

# Function to kill a process and its children
kill_process_tree() {
  local pid=$1
  # Find all child processes and kill them recursively
  pkill -TERM -P $pid
  # Kill the process itself
  kill -TERM $pid
}

# Kill the specified PID and its subprocesses
kill_process_tree $PID



echo "Process $PID and its subprocesses have been terminated."