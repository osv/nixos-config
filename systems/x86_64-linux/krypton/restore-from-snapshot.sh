#!/usr/bin/env bash
set -euo pipefail

# Script to restore ZFS datasets from latest snapshot
# Usage: ./restore-from-snapshot.sh <source-dataset> <destination-dataset>
# Example: ./restore-from-snapshot.sh extbackup/xenon/persist tank/safe/persist

if [ $# -ne 2 ]; then
    echo "Usage: $0 <source-dataset> <destination-dataset>"
    echo ""
    echo "Example:"
    echo "  $0 extbackup/xenon/persist tank/safe/persist"
    echo ""
    echo "This will find the latest snapshot of the source dataset and"
    echo "recursively restore it (including all child datasets) to the destination."
    exit 1
fi

SRC="$1"
DST="$2"

# Verify source dataset exists
if ! zfs list "$SRC" &>/dev/null; then
    echo "Error: Source dataset '$SRC' does not exist"
    echo ""
    echo "Available datasets:"
    zfs list -r "$(echo "$SRC" | cut -d/ -f1)" 2>/dev/null || echo "  Pool not imported?"
    exit 1
fi

# Find latest snapshot of the source dataset (not child datasets)
LATEST=$(zfs list -t snapshot -o name -s creation -H -d 1 "$SRC" 2>/dev/null | tail -1)

if [ -z "$LATEST" ]; then
    echo "Error: No snapshots found for dataset '$SRC'"
    echo ""
    echo "Available snapshots:"
    zfs list -t snapshot -r "$SRC"
    exit 1
fi

echo "====================================="
echo "ZFS Snapshot Restore"
echo "====================================="
echo "Source:      $SRC"
echo "Snapshot:    $LATEST"
echo "Destination: $DST"
echo ""
echo "This will recursively restore all child datasets."
echo ""
read -p "Continue? (y/N) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Aborted."
    exit 1
fi

echo ""
echo "Starting restore..."
echo ""

# Send with -R (recursive, includes all child datasets and their snapshots)
# -F on recv to force rollback if destination exists
zfs send -R "$LATEST" | zfs recv -F "$DST"

echo ""
echo "====================================="
echo "Restore completed successfully!"
echo "====================================="
echo ""
echo "Restored datasets:"
zfs list -r "$DST"
