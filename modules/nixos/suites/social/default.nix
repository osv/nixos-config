{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.suites.social;
in {
  options.nerv.suites.social = with types; {
    enable = mkBoolOpt false "Whether or not to enable social configuration.";
  };

  config = mkIf cfg.enable {
    nerv = {
      opt.apps = {
        telegram = on;
        zoom = on;
        viber = on;
        discord = {
          # enable = yes;
          # native = on;
        };
        # element = on;
      };
    };
  };
}
