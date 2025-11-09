{ config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.system.zfs;

  inherit (lib) mkEnableOption mkIf mkDefault;
  inherit (lib.nerv) mkOpt enabled;
  inherit (lib.types) listOf str;

  my-sync-to-externaldisk =
    pkgs.writeShellScriptBin "my-sync-to-externaldisk" ''
      echo "Press enter to import pool extbackup, create new snapshot in tank/safe and sync to extbackup/$(hostname)"
      read
      if ! zfs list extbackup > /dev/null 2> /dev/null; then
        zpool import -a
        zfs load-key extbackup
      fi
      ${pkgs.sanoid}/bin/syncoid -r \
          --exclude=cache \
          --exclude=docker \
          --exclude=torrents \
          --exclude=stash \
          "$@" tank/safe "extbackup/$(hostname)"
      echo "Press Enter to zpool export extbackup"
      read
      zpool export extbackup
    '';
in {
  options.nerv.opt.system.zfs = {
    enable = mkEnableOption "ZFS support";

    pools = mkOpt (listOf str) [ "tank" ] "The ZFS pools to manage.";

    auto-snapshot = { enable = mkEnableOption "ZFS auto snapshotting"; };
  };

  config = mkIf cfg.enable {
    #  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;

    nerv.opt.common.manuals.files = [ ./README.md ];

    environment.systemPackages = with pkgs; [
      ioztat # small python script for showing per dataset io
      sanoid # for manual backuping
      my-sync-to-externaldisk # My tool for backup to ext hdd
    ];

    services.zfs = {
      trim.enable = yes;
      autoScrub = {
        enable = yes;
        pools = cfg.pools;
      };

      autoSnapshot = mkIf cfg.auto-snapshot.enable {
        enable = yes;
        flags = "-k -p --utc";
        weekly = mkDefault 3;
        daily = mkDefault 3;
        hourly = mkDefault 0;
        frequent = mkDefault 0;
        monthly = mkDefault 2;
      };
    };
  };
}
