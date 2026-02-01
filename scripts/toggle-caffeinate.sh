#!/bin/bash

# PID file to track caffeinate process
PID_FILE="$HOME/.caffeinate.pid"

if [ -f "$PID_FILE" ]; then
    # Caffeinate is running, kill it
    PID=$(cat "$PID_FILE")
    if kill "$PID" 2>/dev/null; then
        rm "$PID_FILE"
        osascript -e 'display notification "Sleep prevention disabled" with title "Caffeinate" sound name "Glass"'
    else
        # PID file exists but process is dead, clean up
        rm "$PID_FILE"
        osascript -e 'display notification "Caffeinate was not running" with title "Caffeinate"'
    fi
else
    # Caffeinate is not running, start it
    caffeinate -sdi &
    echo $! > "$PID_FILE"
    osascript -e 'display notification "Sleep prevention enabled" with title "Caffeinate" sound name "Glass"'
fi
