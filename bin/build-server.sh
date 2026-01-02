#!/bin/bash

# Build the SmSn server
# Can be invoked from any directory (follows symlinks)

set -e

# Resolve symlinks to get the real script location
# This handles both symlinked scripts and scripts in symlinked directories
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd -P )"
PROJECT_DIR="$( cd "${SCRIPT_DIR}/.." && pwd -P )"

echo "Building SmSn server..."
"${PROJECT_DIR}/gradlew" -p "$PROJECT_DIR" :smsn-server:shadowJar

JAR_NAME="smsn-server-1.5-standalone.jar"
JAR_PATH="${PROJECT_DIR}/smsn-server/build/libs/${JAR_NAME}"

if [ -f "$JAR_PATH" ]; then
    echo ""
    echo "Build successful!"
    echo "Server JAR: ${JAR_PATH}"
    echo ""
    echo "To run the server, cd to a directory containing smsn.yaml and run:"
    echo "  ${SCRIPT_DIR}/start-server.sh"
else
    echo "Build failed: JAR not found at ${JAR_PATH}"
    exit 1
fi
