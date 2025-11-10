{ pkgs, lib, modulesPath, inputs, ... }:

with lib;
with lib.nerv;
{
  imports = [
    "${modulesPath}/installer/cd-dvd/installation-cd-base.nix"
    inputs.nixos-generators.nixosModules.install-iso
  ];

  # Disable wireless in favor of NetworkManager
  networking.wireless.enable = mkForce false;
  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.backend = "iwd";
  networking.wireless.iwd.enable = true;

  # Enable experimental features for flakes
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  # Point to nixpkgs for easier installation
  nix.registry.nixpkgs.flake.outPath = builtins.path {
    name = "source";
    path = pkgs.path;
  };

  nerv.opt = {
    user.name = "nixos";

    nix = on;

    apps = {
      chromium = on;
      # firefox = on;
      # vscode = on;
      gparted = on;
    };

    cli-apps = {
      # neovim = on;
      # tmux = on;
    };

    desktop = {
      # xmonad = on;  # Disabled for simpler ISO
      fluxbox = on;
      addons = {
        term = on // {
          defaultTerminal = "wezterm";
          urxvt = on;
          wezterm = on;
        };
        gtk = on;
        mime = on;
        file = on // {
          letsMountWindows = yes;
          letsMountISO = yes;
          consoleFile = on;
          gnomeFile = on;
          kdeFile = on;
        };
        keyring = on // {
          backend = "pass";
        };
      };
    };

    tools = {
      direnv = on;
    };

    hardware = {
      audio = on;
      networking = on;
    };

    services = {
      openssh = on;
      printing = on;
    };

    security = {
      pass = on;
      doas = on;
      keyring = on;
    };

    system = {
      boot = on;
      fonts = on;
      locale = on;
      time = on;
      xkb = on;
      # Add ZFS support for installation
      zfs = on // { auto-snapshot = off; };
    };
  };

  # ISO image configuration
  isoImage = {
    isoBaseName = mkForce "nixos-graphical-installer";
    # Include your flake in the ISO for easier installation
    contents = [
      {
        source = inputs.self;
        target = "/nixos-config/";
      }
    ];
    squashfsCompression = mkForce "zstd -Xcompression-level 10";
  };

  # Helpful message on login
  services.getty.helpLine = ''
    The "nixos" and "root" accounts have empty passwords.

    An ssh daemon is running. Set a password with `passwd`
    or add an ssh key to /home/nixos/.ssh/authorized_keys

    Your NixOS config is available at /nixos-config/

    Type `sudo systemctl start display-manager' to
    start the graphical user interface.
  '';

  # Enable X server and display manager
  services.xserver.enable = true;

  # Use LightDM as display manager
  # services.xserver.displayManager.lightdm.enable = true;

  # Enable Fluxbox window manager
  services.xserver.windowManager.fluxbox.enable = true;

  # Set default session to Fluxbox
  services.displayManager.defaultSession = "none+fluxbox";

  # Override password handling for nixos user (prevent SOPS conflict)
  users.users.nixos.hashedPasswordFile = mkForce null;
  users.users.nixos.initialHashedPassword = mkForce "";

  # Auto-login as nixos
  services.displayManager.autoLogin = {
    enable = true;
    user = "nixos";
  };

  # Add minimal Emacs with nix-mode for editing Nix files
  environment.systemPackages = with pkgs; [
    (emacs30.pkgs.withPackages (epkgs: [
      epkgs.nix-mode
    ]))
  ];

  system.stateVersion = "25.05";
}
