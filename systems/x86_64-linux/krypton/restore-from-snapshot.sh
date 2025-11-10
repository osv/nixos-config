#!/usr/bin/env bash
set -euo pipefail

# Script to restore ZFS datasets from latest snapshot using rsync
# Usage: ./restore-from-snapshot.sh <source-dataset> <destination-path> [--exclude pattern]...
# Example: ./restore-from-snapshot.sh extbackup/xenon/persist /mnt/persist --exclude derivative

if [ $# -lt 2 ]; then
    echo "Usage: $0 <source-dataset> <destination-path> [--exclude pattern]..."
    echo ""
    echo "Examples:"
    echo "  $0 extbackup/xenon/persist /mnt/persist"
    echo "  $0 extbackup/xenon/persist /mnt/persist --exclude derivative"
    echo "  $0 extbackup/xenon/persist /mnt/persist --exclude derivative --exclude cache"
    echo ""
    echo "This will find the latest snapshot of each dataset and"
    echo "recursively restore all nested datasets using rsync."
    echo ""
    echo "Options:"
    echo "  --exclude pattern  Skip datasets containing this pattern (can be used multiple times)"
    exit 1
fi

SRC="$1"
DST="$2"
shift 2

# Parse exclude patterns
EXCLUDES=()
while [[ $# -gt 0 ]]; do
    case $1 in
        --exclude)
            if [ $# -lt 2 ]; then
                echo "Error: --exclude requires an argument"
                exit 1
            fi
            EXCLUDES+=("$2")
            shift 2
            ;;
        *)
            echo "Error: Unknown argument '$1'"
            exit 1
            ;;
    esac
done

# Verify source dataset exists
if ! zfs list "$SRC" &>/dev/null; then
    echo "Error: Source dataset '$SRC' does not exist"
    echo ""
    echo "Available datasets:"
    zfs list -r "$(echo "$SRC" | cut -d/ -f1)" 2>/dev/null || echo "  Pool not imported?"
    exit 1
fi

# Get all datasets under source (including source itself)
DATASETS=$(zfs list -r -H -o name "$SRC" | sort)

# Filter out excluded datasets
if [ ${#EXCLUDES[@]} -gt 0 ]; then
    FILTERED_DATASETS=""
    for dataset in $DATASETS; do
        SKIP=false
        for pattern in "${EXCLUDES[@]}"; do
            if [[ "$dataset" == *"$pattern"* ]]; then
                SKIP=true
                echo "Excluding: $dataset (matches pattern '$pattern')"
                break
            fi
        done
        if [ "$SKIP" = false ]; then
            FILTERED_DATASETS="$FILTERED_DATASETS$dataset"$'\n'
        fi
    done
    DATASETS=$(echo "$FILTERED_DATASETS" | grep -v '^$')
fi

echo "====================================="
echo "ZFS Snapshot Restore (rsync)"
echo "====================================="
echo "Source:      $SRC"
echo "Destination: $DST"
if [ ${#EXCLUDES[@]} -gt 0 ]; then
    echo "Excludes:    ${EXCLUDES[*]}"
fi
echo ""
echo "Found $(echo "$DATASETS" | wc -l) datasets to restore"
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

# Restore each dataset separately
for dataset in $DATASETS; do
    # Find latest snapshot for this specific dataset
    LATEST=$(zfs list -t snapshot -o name -s creation -H -d 1 "$dataset" 2>/dev/null | tail -1 | cut -d@ -f2)

    if [ -z "$LATEST" ]; then
        echo "Warning: No snapshot for $dataset, skipping"
        continue
    fi

    # Get mountpoint of the dataset
    MOUNTPOINT=$(zfs get -H -o value mountpoint "$dataset")

    if [ "$MOUNTPOINT" = "none" ] || [ "$MOUNTPOINT" = "-" ]; then
        echo "Skipping $dataset (no mountpoint)"
        continue
    fi

    # Calculate relative path and destination
    RELPATH=$(echo "$dataset" | sed "s|^$SRC||")
    DEST_PATH="${DST}${RELPATH}"
    SNAP_PATH="${MOUNTPOINT}/.zfs/snapshot/${LATEST}"

    echo "---"
    echo "Dataset:  $dataset"
    echo "Snapshot: $LATEST"
    echo "From:     $SNAP_PATH"
    echo "To:       $DEST_PATH"

    # Create destination directory if needed
    mkdir -p "$DEST_PATH"

    # Rsync from snapshot to destination
    rsync -aHAWX --info=progress2 \
        "${SNAP_PATH}/" \
        "${DEST_PATH}/"

    echo "✓ Completed"
done

echo ""
echo "====================================="
echo "Restore completed successfully!"
echo "====================================="
