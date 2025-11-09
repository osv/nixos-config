{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.suites.video;
in
{
  options.nerv.suites.video = with types; {
    enable = mkBoolOpt false "Whether or not to enable video configuration.";
  };

  config = mkIf cfg.enable {
    nerv = {
      opt.apps = {
        # pitivi = on;
        # obs = on;
        shotcut = on;
        simplescreenrecorder = on;
      };
    };
  };
}
