{ pkgs, inputs, lib, nixos-hardware, config, ... }:

with lib;
with lib.nerv;
let
  isRootOnTmpFS = true;
  discoDevices = import ./disco-zfs.nix {
    ssdDisk = "/dev/disk/by-id/nvme-KINGSTON_SKC3000S1024G_50026B7686493B4A";
    useRootOnTmpFs = isRootOnTmpFS;
  };
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
        # makeLinuxFastAgain = on;
        noWatchDog = on;
        oom = on;
        # warmup.paths = [ "/home/osv/.mozilla" ];
      };
    };
    opt.persist = {
      enable = isRootOnTmpFS;
      persistHome = yes;
    };

    opt.hardware.autorandr = on;

    archetypes = { workstation = on; gaming=off; };
    # Temp
    suites = {
      video = fuckOff;
      social = on;
      media = fuckOff;
    };
    opt.tools = {
      nix-ld = on; # Required for uv/pip binary wheels (pyarrow, etc.)
      appimage-run = fuckOff;
    };
    opt.apps = {
      firefox = on;
      ai = {
        claude-desktop = no;
        code-cursor = no;
        whisper = no;
      };
    };
  };

  # https://ditana.org/docs/the-installer/kernel_configuration/zram/
  zramSwap = {
    enable = yes;
    # This is the size of a block device to be created, in a percentage of ram,
    # it doesnt reflect actual memory that is being used in any way.
    # Compression ratio expected to be at least 3x the size of a ram,
    # so having values above 100 is ok.
    memoryPercent = 50;
    algorithm = "lz4";
  };
  
  boot.kernelParams = [
    # needed for Intel Iris Xe
     "i915.force_probe=46a8"
     "i915.enable_guc=3"
     "i915.fastboot=1"
    # needed for keyboard
    # "i8042.dumbkbd=1"
    # "i8042.nopnp=1"
  ];

  # sops.secrets = {
  #   vpn.sopsFile = ../../../secrets/vpn.yaml;
  #   owner = "osv"; # nerv.opt.user.name;
  # };

  # services.openvpn = {
  #   servers = {
  #     kayzen-development = {
  #       config = "config ${sops.secrets.vpn.kzd.path}";
  #       autoStart = false;
  #       # updateResolvConf = true;

  #     };

  #     kayzen-production = {
  #       config = builtins.readFile ./kayzen-production.ovpn;
  #       autoStart = false;
  #       # updateResolvConf = true;
  #     };
  #   };
  # };

  console = {
    font = "cyr-sun16";
    keyMap = "us";
    earlySetup = true; # Инициализирует консоль до запуска X
    colors = [
      # Normal
      "292d3e" # Black
      "f07178" # Red
      "c3e88d" # Green
      "ffcb6b" # Yellow
      "82aaff" # Blue
      "c792ea" # Magenta
      "89ddff" # Cyan
      "bbc5ff" # White
      # Bright
      "737778" # Black (Bright)
      "ff8b92" # Red (Bright)
      "ddffa7" # Green (Bright)
      "ffe585" # Yellow (Bright)
      "e1acff" # Blue (Bright)
      "ef9ebe" # Magenta (Bright)
      "a3f7ff" # Cyan (Bright)
      "ffffff" # White (Bright)
    ];
  };

  services.fstrim.enable = true;
  services.displayManager = {
    defaultSession = "none+xmonad";
    autoLogin = {
      enable = true;
      user = "osv";
    };
  };
  services.xserver = {
    windowManager.fluxbox.enable = true;
    desktopManager.xterm.enable = true;
  };

  # Гарантировать доступ к TTY при зависании X
  services.logind.settings.Login = {
    NAutoVTs = 6;   # Автоматически запускать getty на 6 VT
    ReserveVT = 6;  # Резервировать VT 6 для getty всегда
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

  # NOTE: NixOS 26.05 uses systemd stage 1 by instead of scripted stage 1
  # This system uses ZFS with encryption (encryption = "on", keylocation = "prompt")
  #
  # Potential issue: systemd stage 1 may have problems with ZFS native encryption
  # If boot fails, temporarily disable systemd stage 1:
  #   boot.initrd.systemd.enable = false;
  #
  # For now, we're keeping systemd stage 1 enabled (default in 26.05)
  # If you experience boot issues with encrypted ZFS, see:
  # https://nixos.org/manual/nixos/stable/release-notes#sec-release-26.05-highlights
  #
  # The old scripted stage 1 is deprecated and will be removed in 26.11

  # No NVIDIA GPU on this system (krypton is Dell Latitude 5530 laptop)

  # environment.systemPackages = [
  #   (pkgs.aider-chat.overridePythonAttrs (oldAttrs: rec {
  #     # append the SDK to the existing deps list
  #     dependencies = oldAttrs.dependencies ++ [ pkgs.python312.google-cloud-aiplatform pkgs.python312.google-generativeai ];
  #   }))
  # ];
  # environment.systemPackages = [ (pkgs.llama-cpp.override { cudaSupport = true; }) ];

  boot.loader.grub.splashImage = lib.mkForce ../../../assets/grub/akira.png;
  boot.loader.grub.backgroundColor = lib.mkForce "#09090B";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}

# tank/local/nix              /nix zfs nodev,nosuid 0 0
# tank/safe/persist           /persist zfs nodev,nosuid 0 0
# tank/safe/persist/home      /home zfs nodev,nosuid 0 0
# tank/safe/persist/home/osv  /home zfs nodev,nosuid 0 0
# tank/docker                 /var/lib/docker zfs nodev,nosuid 0 0
