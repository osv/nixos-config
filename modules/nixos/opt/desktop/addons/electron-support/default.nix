{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.opt.desktop.addons.electron-support;
in {
  options.nerv.opt.desktop.addons.electron-support = with types; {
    enable = mkBoolOpt false
      "Whether to enable electron support in the desktop environment. Wayland";
  };

  config = mkIf cfg.enable {
    nerv.home.configFile."electron-flags.conf".source =
      ./electron-flags.conf;

    environment.sessionVariables = { NIXOS_OZONE_WL = "1"; };
  };
}
