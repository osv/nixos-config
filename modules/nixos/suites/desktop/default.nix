{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.suites.desktop;
in
{
  options.nerv.suites.desktop = with types; {
    enable =
      mkBoolOpt false "Whether or not to enable common desktop configuration.";
  };

  config = mkIf cfg.enable {
    services.libinput.enable = true;
    services.xserver.desktopManager.xterm.enable = true; # fallback when xmonad first compilation failure

    # services.logind = lib.mkDefault {
    #   lidSwitch = "suspend";
    #   lidSwitchExternalPower = "suspend";
    #   lidSwitchDocked="ignore";
    #   extraConfig = ''
    #     KillUserProcesses=no
    #     IdleAction=lock
    #     IdleActionSec=15min
    #   '';
    # };

    nerv = {
      opt.common.manuals = on;

      opt.desktop = {
        xmonad = on;
        # gnome = on;
        # sway = on;
        # displayManager.gdm = on // { wayland = fuckOff; };
        displayManager.sddm = on;
        addons = {
          term = on // {
            defaultTerminal = "wezterm";
            alacritty = on;
            urxvt = on;
            kitty = on;
            wezterm = on;
          };
          gtk = on;
          mime = on;
          file = on // {
            letsMountWindows = yes;
            letsMountISO = yes;
            withSambaBrowse = yes;

            consoleFile = on;
            gnomeFile = on;
            kdeFile = on;
          };
          keyring = on // {
            backend = "pass";
          };
        };
      };

      opt.apps = {
        # _1password = on;
        firefox = on;
        vlc = on;
        # logseq = on;
        # hey = on;
        # pocketcasts = on;
        # yt-music = on;
        # twitter = on;
        gparted = on;
        office = on;
        torrent = on;
      };

      opt.security = {
        pass = on;
      };
    };
  };
}
