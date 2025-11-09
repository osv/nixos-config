{ options, config, lib, pkgs, ... }:
with lib;
with lib.nerv;
let cfg = config.nerv.archetypes.workstation;
in
{
  options.nerv.archetypes.workstation = with types; {
    enable =
      mkBoolOpt false "Whether or not to enable the workstation archetype.";
  };

  config = mkIf cfg.enable {
    nerv = {
      suites = {
        common = on;
        desktop = on;
        development = on;
        art = on;
        video = on;
        social = on;
        media = on;
      };

      opt.tools = {
        appimage-run = on;
      };

      opt.security = {
        vpn = on // {
          kzVPN = yes;
        };
      };
    };
  };
}
