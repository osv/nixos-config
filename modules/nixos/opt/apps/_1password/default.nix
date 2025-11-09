{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.apps._1password;
in
{
  options.nerv.opt.apps._1password = with types; {
    enable = mkBoolOpt false "Whether or not to enable 1password.";
  };

  config = mkIf cfg.enable {
    programs = {
      _1password = on;
      _1password-gui = {
        enable = true;

        polkitPolicyOwners = [ config.nerv.opt.user.name ];
      };
    };
  };
}
