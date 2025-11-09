---
title: "ZFS Tips"
---

# Backup to external hdd raidz1

Yeah, raidz on single HDD, see https://www.youtube.com/watch?v=-wlbvt9tM-Q for original idea.

## Create Zpool

```shell
sudo zpool create extbackup -f -O compression=zstd-5 \
            -O encryption=on -O keylocation=prompt -O keyformat=passphrase \
            -o ashift=12
            raidz1 /dev/sdb2 /dev/sdb3 /dev/sdb4 /dev/sdb5
```

Zstd-5 looks optimal for my HDD and CPU. See https://www.reddit.com/r/HomeServer/comments/sxxd4w/a_simple_real_world_zfs_compression_speed_an/

## Backup

Assuming host's ZFS pool called "tank". See disco-zfs.nix.

To make backup run tool:

```shell
my-sync-to-externaldisk --no-sync-snap --delete-target-snapshots
```

