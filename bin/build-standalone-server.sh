#!/bin/bash

# Build the standalone SmSn server

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_DIR="${SCRIPT_DIR}/.."

echo "Building standalone SmSn server..."
cd "$PROJECT_DIR"
./gradlew :smsn-server:shadowJar

JAR_NAME="smsn-server-1.5-standalone.jar"
JAR_PATH="${PROJECT_DIR}/smsn-server/build/libs/${JAR_NAME}"

if [ -f "$JAR_PATH" ]; then
    echo ""
    echo "Build successful!"
    echo "Standalone server JAR: ${JAR_PATH}"
    echo ""
    echo "To run the server, cd to a directory containing smsn.yaml and run:"
    echo "  ${SCRIPT_DIR}/run-standalone-server.sh"
else
    echo "Build failed: JAR not found at ${JAR_PATH}"
    exit 1
fi
