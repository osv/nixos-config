{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.suites.music;
in
{
  options.nerv.suites.music = with types; {
    enable = mkBoolOpt false "Whether or not to enable music configuration.";
  };

  config = mkIf cfg.enable {
    nerv = {
      opt.apps = {
        ardour = on;
        bottles = on;
      };
    };
  };
}
