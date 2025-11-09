{ pkgs, lib, nixos-hardware, config, ... }:

with lib;
with lib.nerv;
let
  isRootOnTmpFS = true;
  discoDevices = import ./disco-zfs.nix { useRootOnTmpFs = isRootOnTmpFS; };
  persistMountPoints = map (it: it.mountpoint)
    (filter (it: it ? mountpoint && hasPrefix "/persist" it.mountpoint)
      (attrValues discoDevices.disko.devices.zpool.tank.datasets));
in {
  imports = [ ./hardware.nix ];

  disko.devices = discoDevices.disko.devices;

  # Ensure all /persist (root dir, configs, other) are mounted for boot
  fileSystems = listToAttrs ((map (mountPoint: {
    name = mountPoint;
    value = { neededForBoot = true; };
  }) persistMountPoints));

  nerv = {
    opt.system = {
      zfs = on // { auto-snapshot = on; };
      performance = on // {
        makeLinuxFastAgain = on;
        noWatchDog = on;
        oom = on;
        warmup.paths = [ "/home/osv/.mozilla" ];
      };
    };
    opt.persist = {
      enable = isRootOnTmpFS;
      persistHome = yes;
    };

    archetypes = { minimal = on; };
  };

  services.fstrim.enable = true;
  services.xserver = {
    windowManager.fluxbox.enable = true;
    desktopManager.xterm.enable = true;
    displayManager = {
      defaultSession = "none+xmonad";
      autoLogin = {
        enable = true;
        user = "osv";
      };
    };
  };

  fonts.fontconfig = {
    subpixel.rgba = "rgb";
    subpixel.lcdfilter = "light";
    hinting.style = "slight";
    hinting.enable = yes;
    hinting.autohint = yes;
    antialias = yes;
  };

  hardware.bluetooth.enable = true;

  boot.kernelParams = [
    # needed for Intel Iris Xe
    "i915.force_probe=46a8"
    "i915.enable_guc=3"
    "i915.fastboot=1"
    # needed for keyboard
    # "i8042.dumbkbd=1"
    # "i8042.nopnp=1"
  ];

  boot.loader.grub.splashImage = lib.mkForce ../../../assets/grub/akira.png;
  boot.loader.grub.backgroundColor = lib.mkForce "#09090B";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
