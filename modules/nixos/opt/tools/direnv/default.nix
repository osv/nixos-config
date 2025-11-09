{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.opt.tools.direnv;
in
{
  options.nerv.opt.tools.direnv = with types; {
    enable = mkBoolOpt false "Whether or not to enable direnv.";
  };

  config = mkIf cfg.enable {
    nerv = {
      opt.persist.state.homeDirectories = [ ".local/share/direnv" ];

      home.extraOptions = {
        programs.direnv = {
          enable = true;
          nix-direnv = on;
        };
      };
    };
  };
}
