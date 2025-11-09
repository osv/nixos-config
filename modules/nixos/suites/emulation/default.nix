{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.suites.emulation;
in
{
  options.nerv.suites.emulation = with types; {
    enable =
      mkBoolOpt false "Whether or not to enable emulation configuration.";
  };

  config = mkIf cfg.enable {
    nerv = {
      opt.apps = {
        yuzu = on;
        pcsx2 = on;
        dolphin = on;
      };
    };
  };
}
