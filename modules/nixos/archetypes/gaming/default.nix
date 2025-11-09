{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.archetypes.gaming;
in
{
  options.nerv.archetypes.gaming = with types; {
    enable = mkBoolOpt false "Whether or not to enable the gaming archetype.";
  };

  config = mkIf cfg.enable {
    nerv.suites = {
      common = on;
      desktop = on;
      games = on;
      social = on;
      media = on;
    };
  };
}
