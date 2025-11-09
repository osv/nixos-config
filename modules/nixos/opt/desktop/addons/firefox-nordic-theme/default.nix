{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.desktop.addons.firefox-nordic-theme;
  profileDir = ".mozilla/firefox/${config.nerv.opt.user.name}";
in
{
  options.nerv.opt.desktop.addons.firefox-nordic-theme = with types; {
    enable = mkBoolOpt false "Whether to enable the Nordic theme for firefox.";
  };

  config = mkIf cfg.enable {
    nerv.opt.apps.firefox = {
      extraConfig = builtins.readFile
        "${pkgs.nerv.firefox-nordic-theme}/configuration/user.js";
      userChrome = ''
        @import "${pkgs.nerv.firefox-nordic-theme}/userChrome.css";

        /* Assuming "Tree Style Tab" is used, so no need tabs!  */
        #TabsToolbar {
          visibility: collapse !important;
        }
      '';
    };
  };
}
