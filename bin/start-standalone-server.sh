#!/bin/bash

# Run the standalone SmSn server
# Must be invoked from a directory containing smsn.yaml

set -e

# Resolve symlinks to get the real script location
# This handles both symlinked scripts and scripts in symlinked directories
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd -P )"
PROJECT_DIR="$( cd "${SCRIPT_DIR}/.." && pwd -P )"
JAR_NAME="smsn-server-1.5-standalone.jar"
JAR_PATH="${PROJECT_DIR}/smsn-server/build/libs/${JAR_NAME}"

# Build if JAR doesn't exist
if [ ! -f "$JAR_PATH" ]; then
    echo "JAR not found, building..."
    "${PROJECT_DIR}/gradlew" -p "$PROJECT_DIR" :smsn-server:shadowJar
fi

# Check if config exists in current directory
if [ ! -f "smsn.yaml" ]; then
    echo "Error: smsn.yaml not found in current directory."
    echo ""
    echo "Usage: cd /path/to/directory/with/smsn.yaml && $0"
    echo ""
    echo "See README.md for smsn.yaml format."
    exit 1
fi

echo "Starting standalone SmSn server..."
echo "Config: $(pwd)/smsn.yaml"
echo "WebSocket endpoint: ws://localhost:8182/smsn"
echo ""

exec java -jar "$JAR_PATH" "$@"
