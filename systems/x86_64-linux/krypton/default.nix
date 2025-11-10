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

    archetypes = { workstation = on; gaming=off; };
    # Temp
    suites = {
      video = fuckOff;
      social = fuckOff;
      media = fuckOff;
    };
    opt.tools = { appimage-run = fuckOff; };
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
    videoDrivers = [
      # "nouveau"
      "nvidia"
    ];
    # enable nvidia-settings overclocking
    deviceSection = ''
      Option         "Coolbits" "28"
    '';
    # # Set Persistence Mode for all GPUs (so power limits "stick" across WUs)
    # nvidia-smi -pm 1
    #
    # # Set the Power Limit for GPU0 to 160W
    # nvidia-smi -i 0 -pl 160
    #
    # # Set the Minimum GPU Core Clock to 1830MHz and the Maximum to 1980MHz.
    # nvidia-smi -i 0 -lgc 1830,1980
    #
    # # Enable manual fan control
    # # Set both fans for a Turing or later GPU to 75% (Pascal and earlier only have 1 fan control register which controls both fans)
    # # Set GPU0 to "Prefer Maximum Performance"
    # # Add a +75MHz (5 x 15MHz Turing "bins") GPU Clock offset
    # DISPLAY=:0 XAUTHORITY=/run/user/1000/gdm/Xauthority nvidia-settings \
    #  -a [gpu:0]/GPUFanControlState=1 \
    #  -a [fan:0]/GPUTargetFanSpeed=75 \
    #  -a [fan:1]/GPUTargetFanSpeed=75 \
    #  -a [gpu:0]/GPUPowerMizerMode=1 \
    #  -a [gpu:0]/GPUGraphicsClockOffsetAllPerformanceLevels=75
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

  # Nvidia related config
  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.beta;
    open = false;
  };
  boot.extraModprobeConfig = ''
    options nvidia-drm modeset=1
    options nvidia-drm fbdev=1
  '';

  # options nvidia-drm fbdev=1

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
