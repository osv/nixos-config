{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.opt.desktop.fluxbox;
in
{
  options.nerv.opt.desktop.fluxbox = with types; {
    enable = mkBoolOpt false "Whether or not to enable Fluxbox window manager.";
  };

  config = mkIf cfg.enable {
    services.xserver.windowManager.fluxbox.enable = true;
    programs.dconf.enable = true;

    services.dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };

    # Configure Fluxbox to use Windows key (Mod4)
    nerv.home.file.".fluxbox/keys".text = ''
      # Fluxbox keys - Using Mod4 (Windows key) instead of Control

      # Open terminal
      Mod4 Return :Exec wezterm

      # Open application launcher
      Mod4 p :Exec ${pkgs.rofi}/bin/rofi -matching fuzzy -modi combi -show combi -combi-modi run,drun || ${pkgs.xterm}/bin/xterm

      # Window management
      Mod4 q :Close
      Mod4 f :Fullscreen
      Mod4 m :Maximize

      # Workspace switching
      Mod4 1 :Workspace 1
      Mod4 2 :Workspace 2
      Mod4 3 :Workspace 3
      Mod4 4 :Workspace 4

      # Move window to workspace
      Mod4 Shift 1 :SendToWorkspace 1
      Mod4 Shift 2 :SendToWorkspace 2
      Mod4 Shift 3 :SendToWorkspace 3
      Mod4 Shift 4 :SendToWorkspace 4

      # Window cycling
      Mod4 Tab :NextWindow
      Mod4 Shift Tab :PrevWindow

      # Restart Fluxbox
      Mod4 Shift r :Restart

      # Exit Fluxbox
      Mod4 Shift e :Exit

      # Right-click menu
      OnDesktop Mouse3 :RootMenu
    '';

    environment.systemPackages = with pkgs; [
      fluxbox
      rofi
    ];
  };
}
