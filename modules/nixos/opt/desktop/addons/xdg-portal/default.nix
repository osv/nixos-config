{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.opt.desktop.addons.xdg-portal;
in {
  options.nerv.opt.desktop.addons.xdg-portal = with types; {
    enable = mkBoolOpt false "Whether or not to add support for xdg portal.";
  };

  config = mkIf cfg.enable {
    xdg = {
      portal = {
        enable = true;
        extraPortals = with pkgs;
          [
            xdg-desktop-portal-wlr
            # xdg-desktop-portal-gtk
          ];
        # gtkUsePortal = true;
      };
    };
  };
}
