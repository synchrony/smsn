#!/bin/bash

# Build the standalone SmSn server (without TinkerPop/Gremlin Server dependencies)

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "${DIR}/.."

echo "Building standalone SmSn server..."
./gradlew :smsn-server:shadowJar

JAR_PATH="smsn-server/build/libs/smsn-server-standalone.jar"

if [ -f "$JAR_PATH" ]; then
    echo ""
    echo "Build successful!"
    echo "Standalone server JAR: ${JAR_PATH}"
    echo ""
    echo "To run the server:"
    echo "  java -jar ${JAR_PATH} -c smsn.yaml"
    echo ""
    echo "Options:"
    echo "  -p, --port PORT     Server port (default: 8182)"
    echo "  -h, --host HOST     Server host (default: 0.0.0.0)"
    echo "  -c, --config FILE   Configuration file (smsn.yaml)"
    echo "      --help          Show help message"
else
    echo "Build failed: JAR not found at ${JAR_PATH}"
    exit 1
fi
