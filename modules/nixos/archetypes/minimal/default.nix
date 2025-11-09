{ options, config, lib, pkgs, ... }:
with lib;
with lib.nerv;
let cfg = config.nerv.archetypes.minimal;
in
{
  options.nerv.archetypes.minimal = with types; {
    enable =
      mkBoolOpt false "Minimal but with X11 archetype.";
  };

  config = mkIf cfg.enable {
    nerv = {
      suites = {
        common = on;
        desktop = on;
        development = on;
      };
    };
  };
}
