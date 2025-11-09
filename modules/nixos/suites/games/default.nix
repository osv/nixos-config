{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.suites.games;
in
{
  options.nerv.suites.games = with types; {
    enable =
      mkBoolOpt false "Whether or not to enable common games configuration.";
  };

  config = mkIf cfg.enable {
    nerv = {
      opt.apps = {
        prismlauncher = on;
        lutris = on;
        winetricks = on;
        protontricks = on;
        # doukutsu-rs = on; broken?
        bottles = on;
      };
      opt.gaming = {
        steam = on;
        optimization = on;
      };
      opt.cli-apps = {
        wine = on;
        proton = on;
      };
      opt.tools = {
        nix-ld = on;
      };
    };
  };
}
