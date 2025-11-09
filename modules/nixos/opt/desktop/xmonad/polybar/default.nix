{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.desktop.xmonad.polybar;
  term = config.nerv.opt.desktop.addons.term;
  substitutedConfig = pkgs.replaceVars ./config {
    term = term.defaultTerminal;
  };
in {
  options.nerv.opt.desktop.xmonad.polybar = with types; {
    enable =
      mkBoolOpt false "Whether or not to enable Polybar config for Xmonad.";
  };

  config = mkIf cfg.enable {
    # https://gvolpe.com/blog/xmonad-polybar-nixos/

    nerv.home.configFile."xmonad-polybar" = {
      source = ./config;
      recursive = true;
    };

    services.upower.enable = true;
    systemd.services.upower.enable = true;

    environment.systemPackages = with pkgs; [
      xmonadctl # polybar to xmonad connect
      polybarFull
      gucharmap # find char of font for polybar
      zenity # M-F1 Help Dialog
    ];
  };
}

