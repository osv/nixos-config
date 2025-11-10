{ ssdDisk ? "/dev/disk/by-id/nvme-KINGSTON_SKC3000S1024G_50026B7686493B4A",
  # I'm booting using tmpfs on /, but for install system I'm using root on zfs (in case of low memory problem). Also root on zfs
  # Also, good to create dataset during installation root (tank/system/root) as fallback option
  # nix run github:nix-community/disko -- -m mount ./systems/x86_64-linux/xenon/disco-zfs.nix --arg useRootOnTmpFs false
  useRootOnTmpFs ? true,
  ... }:
let
  noMount = moreOptions: {
    type = "zfs_fs";
    options = { mountpoint = "none"; } // moreOptions;
  };
  mount = mountPoint: moreOptions: {
    type = "zfs_fs";
    options = { mountpoint = "legacy"; } // moreOptions;
    mountOptions = ["nodev" "nosuid"];
    mountpoint = mountPoint;
  };
  optionalAttrs = cond: as: if cond then as else {};
in
{
  disko.devices = {
    disk = {
      main = {
        type = "disk";
        device = ssdDisk;
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              type = "EF00";
	          start = "0";
              end = "1G";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [ "umask=0077" ];
              };
            };
            zfs = { start = "1G";
                    end = "100%";
                    label = "zfs";
                    content = {
                      type = "zfs";
                      pool = "tank";
                    };
                  };
          };
        };
      };
    };

    zpool = {
    tank = {
      type = "zpool";
      options = {
        ashift = "12";              # Check you drive specs for sector size, ashift=12 for 4k sector
      };
      rootFsOptions = {
        compression = "on";
        canmount = "off";
        mountpoint = "legacy";
        normalization = "formD";  # Preventing duplicate files with names that look exactly the same when using special characters
        relatime = "on";          # Update access time within the past 24hr or on file modify.
        xattr = "sa";             # Store the extended attributes in the inode, so no additional read is needed to get the posixacls on the file
        acltype = "posixacl";     # Allows you to store additional access rights (per user and/or per group) on files
        dnodesize = "auto";       # Useful if the dataset uses the xattr=sa
        encryption = "on";
        keylocation = "prompt";   # type password on boot and then autologin to desktop env
        keyformat = "passphrase";
      };
      # mountpoint = "none";

      # Note: recordsize 1M is fine because no partial record writes in xdg dirs like Downloads
      # or even torrents, See https://www.reddit.com/r/zfs/comments/tmio9p/comment/i1ygtni/
      # But for "work/" directory I'm expecting partial writes,
      # not by editor but sql db sometimes, so lets keep it default 128k
      # Note: Record size for /nix can be max possible. Below is count of files and file size per possible recordsize
      #   4KB  1506295     2GB
      #   8KB   163985     1GB
      #  16KB   114386     1GB
      #  32KB    81567     2GB
      #  64KB    53584     2GB
      # 128KB    34584     3GB
      # 256KB    19339     3GB
      # 512KB    11055     4GB
      #   1MB     5402     4GB
      #   2MB     3409     5GB
      #   4MB     2451     6GB
      #   8MB     1089     6GB
      #  16MB      820     9GB
      #  32MB      253     6GB
      #  64MB      125     5GB
      # 128MB       59     5GB
      # 256MB       34     6GB
      # 512MB       36    11GB
      #   1GB        1   522MB
      datasets =
        (optionalAttrs (!useRootOnTmpFs) {
          "system/root" =                         mount "/"                                 {                                                              "com.sun:auto-snapshot" = "false"; };
          "safe/home" =                           mount "/home"                             { };
        }) // {
        "system" = noMount { };
        "system/nix" =                            mount "/nix"                              { atime="off"; dedup="on"; recordsize="1M"; };
        # Reserve 15% to make it less fragmented
        "reserved" = noMount { reservation="150G"; };
        "safe" = noMount                                                                    {                                                              "com.sun:auto-snapshot" = "true"; };

        "safe/persist" =                          mount "/persist"                          { };
        "safe/persist/cache" =                    mount "/persist/cache"                    { atime="off";                                                 "com.sun:auto-snapshot" = "false"; };
        "safe/persist/derivative" =               mount "/persist/derivative"               { };
        "safe/persist/state" =                    mount "/persist/state"                    { };
        "safe/persist/state/home" =               mount "/persist/state/home"               { };
        "safe/persist/state/home/osv" =           mount "/persist/state/home/osv"           { };
        "safe/persist/state/home/osv/Desktop" =   mount "/persist/state/home/osv/Desktop"   { };
        "safe/persist/state/home/osv/Documents" = mount "/persist/state/home/osv/Documents" {                          recordsize="1M"; };
        "safe/persist/state/home/osv/Downloads" = mount "/persist/state/home/osv/Downloads" {                          recordsize="1M"; compression="off"; "com.sun:auto-snapshot" = "false"; };
        "safe/persist/state/home/osv/Music" =     mount "/persist/state/home/osv/Music"     {                          recordsize="1M"; compression="off"; };
        "safe/persist/state/home/osv/Pictures" =  mount "/persist/state/home/osv/Pictures"  {                          recordsize="1M"; compression="off"; };
        "safe/persist/state/home/osv/Videos" =    mount "/persist/state/home/osv/Videos"    {                          recordsize="1M"; compression="off"; };
        "safe/persist/state/home/osv/work" =      mount "/persist/state/home/osv/work"      { };
        "safe/persist/state/home/osv/torrents" =  mount "/persist/state/home/osv/torrents"  {                          recordsize="1M"; "com.sun:auto-snapshot" = "false"; };
        # stash is smtg that I don't want to backup
        "safe/persist/state/home/osv/stash" =     mount "/persist/state/home/osv/stash"     {                          recordsize="1M";                    "com.sun:auto-snapshot" = "false";  };
        "safe/docker" =                           mount "/var/lib/docker"                   {                                                              "com.sun:auto-snapshot" = "false";  };
        "safe/db" =                               mount "/db"                               { };
        # TODO: https://shatteredsilicon.net/mysql-mariadb-innodb-on-zfs/
        # 16k because of https://mariadb.com/kb/en/innodb-system-variables/#innodb_page_size
        "safe/db/mysql" =                         mount "/db/mysql"                         { atime="off";             recordsize="16k"; };

        # "local" = noMount { };
        # # "local/blank-root" = mount "/";
        # "local/nix" =                             mount "/nix" { dedup="on"; atime="off"; };
        # "safe" = noMount { };
        # "safe/home" =                             mount "/home" { };
        # "safe/persist" =                          mount "/persist" { };
        # "safe/docker" =                           mount "/var/lib/docker" { };
      };
    };
  };

  nodev = optionalAttrs useRootOnTmpFs {
    "/" = {
      fsType = "tmpfs"; mountOptions = [ "size=4G" "defaults" "mode=755" ];
    };
  };
  };
}
