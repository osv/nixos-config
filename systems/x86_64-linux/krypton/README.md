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
# This finds the latest snapshot and recursively restores all nested datasets
cd systems/x86_64-linux/krypton
sudo ./restore-from-snapshot.sh extbackup/xenon/persist tank/safe/persist

# Step 4: Install NixOS
sudo nixos-install --flake .#krypton --root /mnt

# Step 5: Cleanup - export backup pool
sudo zpool export extbackup

echo "Installation complete! You can reboot into krypton."
```

**Alternative: Manual ZFS Send/Recv**

If you prefer to do it manually without the helper script:

```bash
# After disko
sudo nix run github:nix-community/disko -- --mode disko --flake .#krypton

# Import backup
sudo zpool import extbackup
sudo zfs load-key extbackup

# Find latest snapshot and restore recursively
LATEST=$(zfs list -t snapshot -o name -s creation -H -d 1 extbackup/xenon/persist | tail -1)
echo "Restoring from snapshot: $LATEST"

# -R flag includes all child datasets recursively
sudo zfs send -R "$LATEST" | sudo zfs recv -F tank/safe/persist

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

3. **Data restore**: Uses ZFS send/recv to restore data from latest snapshot
   - `restore-from-snapshot.sh` script finds the latest snapshot automatically
   - `-R` flag in `zfs send` includes all child datasets recursively
   - Preserves all ZFS properties, snapshots, and dataset hierarchy
   - Each nested dataset (persist, persist/home, persist/home/osv, etc.) is restored correctly

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
sudo ./restore-from-snapshot.sh extbackup/xenon/persist/home tank/safe/persist/home

# Or manually:
# LATEST=$(zfs list -t snapshot -o name -s creation -H -d 1 extbackup/xenon/persist/home | tail -1)
# sudo zfs send -R "$LATEST" | sudo zfs recv -F tank/safe/persist/home

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
