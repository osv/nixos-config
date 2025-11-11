# Krypton System Installation

Complete installation workflow for Krypton system with data restore from backup.

## Prerequisites

- NixOS installation ISO booted
- This flake repository available (USB drive, git clone, etc.)
- External backup disk with ZFS pool `extbackup` containing backed up data
- Target disk: `/dev/disk/by-id/nvme-KINGSTON_SKC3000S1024G_50026B7686493B4A`

## Installation Steps

### Full Installation Script

```bash
#!/bin/bash
# Complete Krypton Installation with Data Restore

# Step 1: Partition disks and create ZFS pool with disko
sudo nix run github:nix-community/disko -- --mode disko --flake .#krypton

# Step 2: Import backup pool
sudo zpool import extbackup
sudo zfs load-key extbackup
sudo zfs mount -a

# Step 3: Restore data using the helper script
# This finds the latest snapshot of each dataset and restores using rsync
# Exclude derivative dataset (cache data that can be regenerated)
cd systems/x86_64-linux/krypton
sudo ./restore-from-snapshot.sh extbackup/xenon/persist /mnt/persist --exclude derivative

# Step 4: Install NixOS
sudo git config --global --add safe.directory /home/nixos/nixos-config
sudo nixos-install --flake .#krypton --root /mnt

# Step 5: Cleanup - export backup pool
sudo zpool export extbackup

# Step 6: Fix permissions (nixos user id is same like for osv user)
sudo chown nixos -R  /mnt/persist/state/home/osv/

echo "Installation complete! You can reboot into krypton."
```

**Excluding Datasets**

You can exclude specific datasets from restore using `--exclude` (useful for skipping cache/derivative data):

```bash
# Exclude derivative only
sudo ./restore-from-snapshot.sh extbackup/xenon/persist /mnt/persist --exclude derivative

# Exclude multiple datasets
sudo ./restore-from-snapshot.sh extbackup/xenon/persist /mnt/persist \
  --exclude derivative --exclude cache --exclude torrents

# Common exclusions for faster restore:
# - derivative: Package manager caches (can be regenerated)
# - cache: Temporary cache files
# - torrents: Large torrent data
# - stash: Non-backed-up personal data
```

**Alternative: Manual Rsync for Each Dataset**

If you prefer to do it manually without the helper script:

```bash
# After disko
sudo nix run github:nix-community/disko -- --mode disko --flake .#krypton

# Import backup
sudo zpool import extbackup
sudo zfs load-key extbackup
sudo zfs mount -a

# Restore each dataset manually
for dataset in $(zfs list -r -H -o name extbackup/xenon/persist); do
    LATEST=$(zfs list -t snapshot -o name -s creation -H -d 1 "$dataset" | tail -1 | cut -d@ -f2)
    MOUNTPOINT=$(zfs get -H -o value mountpoint "$dataset")
    RELPATH=$(echo "$dataset" | sed 's|extbackup/xenon/persist||')

    if [ -n "$LATEST" ] && [ "$MOUNTPOINT" != "none" ]; then
        echo "Restoring $dataset @ $LATEST"
        rsync -aHAWX --info=progress2 \
            "${MOUNTPOINT}/.zfs/snapshot/${LATEST}/" \
            "/mnt/persist${RELPATH}/"
    fi
done

# Install and cleanup
sudo nixos-install --flake .#krypton --root /mnt
sudo zpool export extbackup
```

### Step-by-Step Explanation

1. **Disko partitioning**: Creates ZFS pool `tank` with all datasets defined in `disco-zfs.nix`
   - All mountpoints are created under `/mnt` (e.g., `/mnt/persist`)

2. **Import backup**: Imports the external backup pool
   - Requires encryption key if backup pool is encrypted
   - Loads the key to unlock encrypted datasets

3. **Data restore**: Uses rsync to restore data from latest snapshot of each dataset
   - `restore-from-snapshot.sh` script automatically finds and restores all nested datasets
   - Loops through each dataset and finds its latest snapshot
   - Uses rsync to copy files while preserving permissions, attributes, and hard links
   - Each nested dataset (persist, persist/home, persist/home/osv, etc.) is restored from its own snapshot

4. **NixOS install**: Installs the system with your flake configuration

5. **Cleanup**: Safely exports the backup pool

### Alternative: Install Without Restore

If you want a fresh system without restoring data:

```bash
# Just run disko and install
sudo nix run github:nix-community/disko -- --mode disko --flake .#krypton
sudo nixos-install --flake .#krypton --root /mnt
```

### Alternative: Install With Selective Restore

To restore only specific datasets (e.g., just home directory):

```bash
# After disko
sudo nix run github:nix-community/disko -- --mode disko --flake .#krypton

# Import backup
sudo zpool import extbackup
sudo zfs load-key extbackup

# Restore only specific dataset (and its children)
cd systems/x86_64-linux/krypton
sudo ./restore-from-snapshot.sh extbackup/xenon/persist/home /mnt/persist/home

# Restore home but exclude cache directories
sudo ./restore-from-snapshot.sh extbackup/xenon/persist/home /mnt/persist/home --exclude cache

# Or manually with rsync:
# LATEST=$(zfs list -t snapshot -o name -s creation -H -d 1 extbackup/xenon/persist/home | tail -1 | cut -d@ -f2)
# MOUNTPOINT=$(zfs get -H -o value mountpoint extbackup/xenon/persist/home)
# sudo rsync -aHAWX "${MOUNTPOINT}/.zfs/snapshot/${LATEST}/" /mnt/persist/home/

# Continue with install
sudo nixos-install --flake .#krypton --root /mnt
sudo zpool export extbackup
```

## Post-Installation

After rebooting into the new system:

1. Verify ZFS pools are mounted:
   ```bash
   zpool status
   zfs list
   ```

2. Check that persistence is working:
   ```bash
   ls -la /persist
   ls -la /home/osv
   ```

3. Verify system configuration:
   ```bash
   nixos-rebuild switch --flake .#krypton
   ```

## Backup Creation

To create backups from krypton to external disk, use the built-in script:

```bash
my-sync-to-externaldisk
```

This script:
- Imports the `extbackup` pool
- Creates snapshots of `tank/safe`
- Syncs to `extbackup/krypton` using syncoid
- Exports the backup pool when done

## Notes

- Krypton uses tmpfs root (`isRootOnTmpFS = true`)
- Persistent data is stored in `/persist` on ZFS
- ZFS auto-snapshots are enabled (weekly, daily, monthly)
- System state version: 24.05
