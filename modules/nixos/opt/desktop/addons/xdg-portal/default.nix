{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;

let cfg = config.nerv.opt.desktop.addons.xdg-portal;
in {
  options.nerv.opt.desktop.addons.xdg-portal = with types; {
    enable = mkBoolOpt false "Whether or not to add support for xdg portal.";
  };

  config = mkIf cfg.enable {
    xdg.portal = {
      enable = true;
      # gtk backend provides FileChooser, Settings (theme/color-scheme), OpenURI,
      # AppChooser, etc. — needed by flatpak apps and useful generally, works on
      # both X11 and Wayland. wlr backend adds ScreenCast/Screenshot on wlroots.
      extraPortals = with pkgs; [ xdg-desktop-portal-gtk xdg-desktop-portal-wlr ];
      # Prefer gtk for the common interfaces; fall back to wlr for ScreenCast/Screenshot.
      config.common.default = [ "gtk" "wlr" ];
    };
  };
}
