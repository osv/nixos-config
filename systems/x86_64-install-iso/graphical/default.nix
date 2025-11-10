{ pkgs, lib, ... }:

with lib;
with lib.internal;
{
  # `install-iso` adds wireless support that
  # is incompatible with networkmanager.
  networking.wireless.enable = mkForce false;

  nerv.opt = {
    nix = on;

    apps = {
      chromium = on;
      firefox = enabled;
      vscode = enabled;
      gparted = enabled;
    };

    cli-apps = {
      # tmux = enabled;
    };

    desktop = {
      xmonad = on;
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
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}
