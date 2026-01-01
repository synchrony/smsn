#!/bin/bash

# Stop the standalone SmSn server

if pkill -f "smsn-server.*standalone" 2>/dev/null; then
    echo "SmSn server stopped."
else
    echo "SmSn server was not running."
fi
