{ config, lib, pkgs, ... }:

with lib;
with lib.nerv;

let
  cfg = config.nerv.opt.apps.mpv;
in
{
  options.nerv.opt.apps.mpv = with types; {
    enable = mkBoolOpt false "Whether or not to enable mpv (media player).";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      (pkgs.mpv.override {
        scripts = with pkgs.mpvScripts; [ uosc ];
      })
    ];

    nerv.home.extraOptions = {
      xdg.configFile."mpv/input.conf".text = ''
        :     script-binding uosc/menu
        alt+x script-binding uosc/menu
        o     script-binding uosc/open-file
        tab   script-binding uosc/toggle-ui
        F1    script-message uosc-keybinds
      '';
    };
  };
}
