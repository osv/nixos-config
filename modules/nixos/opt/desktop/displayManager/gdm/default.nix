{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.desktop.displayManager.gdm;
in
{
  options.nerv.opt.desktop.displayManager.gdm = with types; {
    enable =
      mkBoolOpt false "Whether or not to use Gnome as the desktop environment.";
    wayland = mkBoolOpt false "Whether or not to use Wayland.";
    suspend =
      mkBoolOpt true "Whether or not to suspend the machine after inactivity.";
  };

  config = mkIf cfg.enable {
    nerv.opt.desktop.addons = { gtk = on; };

    services.xserver = {
      enable = true;

      displayManager.gdm = {
        enable = true;
        wayland = cfg.wayland;
        autoSuspend = cfg.suspend;
      };
    };
  };
}
