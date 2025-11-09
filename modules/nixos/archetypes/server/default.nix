{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.archetypes.server;
in
{
  options.nerv.archetypes.server = with types; {
    enable =
      mkBoolOpt false "Whether or not to enable the server archetype.";
  };

  config = mkIf cfg.enable {
    nerv = {
      suites = {
        common-slim = on;
      };

      opt.cli-apps = {
        tmux = on;
      };
    };
  };
}
