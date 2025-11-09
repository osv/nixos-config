{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.apps.office;
in
{
  options.nerv.opt.apps.office = with types; {
    enable = mkBoolOpt false "Whether or not to enable office apps.";
    libreoffice = mkBoolOpt true "Whether or not to install libreoffice.";
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.libreoffice) {
      environment.systemPackages = with pkgs; [ libreoffice ];
      nerv.opt.persist.state.homeDirectories = [ ".config/libreoffice" ];
    })
  ];
}
