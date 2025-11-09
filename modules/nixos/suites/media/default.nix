{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.suites.media;
in
{
  options.nerv.suites.media = with types; {
    enable = mkBoolOpt false "Whether or not to enable media configuration.";
  };

  config = mkIf cfg.enable { nerv = { opt.apps = { freetube = on; }; }; };
}
