{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.apps.ubports-installer;
in
{
  options.nerv.opt.apps.ubports-installer = with types; {
    enable = mkBoolOpt false "Whether or not to enable the UBPorts Installer.";
  };

  config =
    mkIf cfg.enable {
      environment.systemPackages = with pkgs.nerv; [
        ubports-installer
      ];

      services.udev.packages = with pkgs.nerv; [
        ubports-installer-udev-rules
      ];
    };
}
