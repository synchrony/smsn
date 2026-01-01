#!/bin/bash

# Run the standalone SmSn server

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "${DIR}/.."

JAR_PATH="smsn-server/build/libs/smsn-server-standalone.jar"

# Build if JAR doesn't exist
if [ ! -f "$JAR_PATH" ]; then
    echo "JAR not found, building..."
    ./gradlew :smsn-server:shadowJar
fi

# Default config file
CONFIG_FILE="smsn.yaml"

# Check if config exists
if [ ! -f "$CONFIG_FILE" ]; then
    echo "Warning: Configuration file '${CONFIG_FILE}' not found."
    echo "The server will use default configuration."
    echo ""
fi

echo "Starting standalone SmSn server..."
echo "WebSocket endpoint: ws://localhost:8182/smsn"
echo ""

# Pass all arguments to the server
exec java -jar "$JAR_PATH" "$@"
