#!/bin/bash

# Run the standalone SmSn server
# Must be invoked from a directory containing smsn.yaml

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_DIR="${SCRIPT_DIR}/.."
JAR_NAME="smsn-server-1.5-standalone.jar"
JAR_PATH="${PROJECT_DIR}/smsn-server/build/libs/${JAR_NAME}"

# Build if JAR doesn't exist
if [ ! -f "$JAR_PATH" ]; then
    echo "JAR not found, building..."
    (cd "$PROJECT_DIR" && ./gradlew :smsn-server:shadowJar)
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
