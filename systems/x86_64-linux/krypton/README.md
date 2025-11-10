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

# Step 2: Import backup pool (read-only for safety)
sudo zpool import -o readonly=on extbackup
sudo zfs load-key extbackup

# Step 3: Find latest snapshot and copy all data
LATEST=$(ls -t /extbackup/xenon/persist/.zfs/snapshot | head -1)
echo "Restoring from snapshot: $LATEST"

# rsync crosses dataset boundaries and copies everything
sudo rsync -aHAWX --info=progress2 \
  "/extbackup/xenon/persist/.zfs/snapshot/$LATEST/" \
  /mnt/persist/

# Step 4: Install NixOS
sudo nixos-install --flake .#krypton --root /mnt

# Step 5: Cleanup - export backup pool
sudo zpool export extbackup

echo "Installation complete! You can reboot into krypton."
```

### Step-by-Step Explanation

1. **Disko partitioning**: Creates ZFS pool `tank` with all datasets defined in `disco-zfs.nix`
   - All mountpoints are created under `/mnt` (e.g., `/mnt/persist`)

2. **Import backup**: Mounts the external backup pool read-only for safety
   - Requires encryption key if backup pool is encrypted

3. **Data restore**: Uses rsync to copy all data from latest snapshot
   - `-aHAWX` preserves all permissions, attributes, hard links, ACLs, and xattrs
   - Without `-x` flag, rsync crosses ZFS dataset boundaries automatically
   - Copies all subdatasets (persist, persist/home, persist/home/osv, etc.)

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

To restore only specific directories:

```bash
# After disko
sudo nix run github:nix-community/disko -- --mode disko --flake .#krypton

# Import backup
sudo zpool import -o readonly=on extbackup
sudo zfs load-key extbackup

LATEST=$(ls -t /extbackup/xenon/persist/.zfs/snapshot | head -1)

# Restore only home directory
sudo rsync -aHAWX --info=progress2 \
  "/extbackup/xenon/persist/.zfs/snapshot/$LATEST/home/" \
  /mnt/persist/home/

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
