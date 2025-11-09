{ options, config, pkgs, lib, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.opt.tools.qmk;
in
{
  options.nerv.opt.tools.qmk = with types; {
    enable = mkBoolOpt false "Whether or not to enable QMK";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      qmk
    ];

    services.udev.packages = with pkgs; [
      qmk-udev-rules
    ];
  };
}
