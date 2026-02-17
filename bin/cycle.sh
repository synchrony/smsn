#!/bin/bash

# Cycle the SmSn index: back up the current index and clear it for a fresh rebuild.
# Must be invoked from a directory containing smsn.yaml.
# The server will rebuild the index from .smsn source files on next startup.
#
# Usage: cycle.sh [--backups N]
#   --backups N   Number of rolling backups to keep (default: 3)

set -e

NUM_BACKUPS=3

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --backups)
            NUM_BACKUPS="$2"
            shift 2
            ;;
        --help)
            echo "Usage: $0 [--backups N]"
            echo ""
            echo "Cycle the SmSn index: back up and clear for a fresh rebuild."
            echo "Must be run from a directory containing smsn.yaml."
            echo ""
            echo "Options:"
            echo "  --backups N   Number of rolling backups to keep (default: 3)"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--backups N]"
            exit 1
            ;;
    esac
done

# Check for smsn.yaml in current directory
if [ ! -f "smsn.yaml" ]; then
    echo "Error: smsn.yaml not found in current directory."
    echo ""
    echo "Usage: cd /path/to/directory/with/smsn.yaml && $0"
    exit 1
fi

# Parse indexDirectory from smsn.yaml
INDEX_DIR=$(sed -n 's/^indexDirectory:[[:space:]]*//p' smsn.yaml | tr -d '[:space:]')

if [ -z "$INDEX_DIR" ]; then
    echo "Error: indexDirectory not found in smsn.yaml."
    exit 1
fi

# Resolve relative paths against CWD
if [[ "$INDEX_DIR" != /* ]]; then
    INDEX_DIR="$(pwd)/$INDEX_DIR"
fi

BACKUP_DIR="$(pwd)/bk"

echo "Cycling SmSn index..."
echo "  Index directory: $INDEX_DIR"
echo "  Backup directory: $BACKUP_DIR"
echo ""

# Create backup directory if it doesn't exist
mkdir -p "$BACKUP_DIR"

# Rotate backups: delete oldest, shift others
if [ -d "$INDEX_DIR" ]; then
    echo "Rotating backups (keeping $NUM_BACKUPS)..."

    # Delete oldest backup
    if [ -d "$BACKUP_DIR/index.$NUM_BACKUPS" ]; then
        rm -rf "$BACKUP_DIR/index.$NUM_BACKUPS"
        echo "  Deleted oldest backup (index.$NUM_BACKUPS)"
    fi

    # Shift existing backups: index.2 -> index.3, index.1 -> index.2, etc.
    for ((i=NUM_BACKUPS-1; i>=1; i--)); do
        next=$((i+1))
        if [ -d "$BACKUP_DIR/index.$i" ]; then
            mv "$BACKUP_DIR/index.$i" "$BACKUP_DIR/index.$next"
            echo "  Moved index.$i -> index.$next"
        fi
    done

    # Move current index to backup.1
    mv "$INDEX_DIR" "$BACKUP_DIR/index.1"
    echo "  Backed up current index to index.1"
else
    echo "No existing index to back up"
fi

# Create empty index directory (server will initialize it on startup)
mkdir -p "$INDEX_DIR"

echo ""
echo "Cycle complete. Index has been cleared."
echo "Existing backups:"
ls -d "$BACKUP_DIR"/index.* 2>/dev/null | while read -r d; do echo "  $(basename "$d")"; done || echo "  (none)"
echo ""
echo "Start the server to rebuild the index from .smsn source files."
