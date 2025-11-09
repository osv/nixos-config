{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.suites.art;
in
{
  options.nerv.suites.art = with types; {
    enable = mkBoolOpt false "Whether or not to enable art configuration.";
  };

  config = mkIf cfg.enable {
    nerv = {
      opt.apps = {
        gimp = on;
        # inkscape = on;
        # blender = on;
      };
    };
  };
}
