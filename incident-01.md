# Incident:

Not able to boot:

```
PANIC: zfs: adding existent segment to range tree
```

- See https://github.com/openzfs/zfs/issues/15915

# Logbook

## Boot using live cd

## Disable power saving (sleep)

```
sudo systemctl mask sleep.target suspend.target hibernate.target hybrid-sleep.target
# if KDE livecd
systemctl --user stop plasma-powerdevil.service
```

## Backup: Mount broken zfs

```
sudo zpool import -o readonly=on tank
sudo zfs load-key tank
```

Mount flat directory (you cannot mount if parent dataset directory is readonly). run next bash script

```bash
set -e
set -x


POOL="tank"
BASE_MOUNT_ROOT="/mnt/baddisk"

# Function to convert dataset name to flat directory name
flat_mountpoint() {
  echo "$1" | tr '/' '_'
}

# Step 1: Create all mount directories first
zfs list -H -o name -r "$POOL" | while read -r fs; do
  if [ "$(zfs get -H -o value mountpoint "$fs")" = "legacy" ]; then
    dir="$BASE_MOUNT_ROOT/$(flat_mountpoint "$fs")"
    mkdir -p "$dir"
  fi
done

# Step 2: Mount all datasets
zfs list -H -o name -r "$POOL" | while read -r fs; do
  if [ "$(zfs get -H -o value mountpoint "$fs")" = "legacy" ]; then
    dir="$BASE_MOUNT_ROOT/$(flat_mountpoint "$fs")"
    echo "Mounting $fs at $dir"
    sudo mount -t zfs "$fs" "$dir"
  fi
done
```

## Unmount not needed datasets

## Mount backup disk

```
sudo zfs import extbackup
sudo zfs load-key extbackup
sudo zfs create extbackup/xenon-2025-05-20
```

## Copy files

```
rsync -aHAX --progress /mnt/baddisk/ extbackup/xenon-2025-05-20/
```

## Boot

When the GRUB menu appears, press `e` to edit the boot entry.

```
zfs.zfs_recover=1
```

Added this option to kernel params in config
