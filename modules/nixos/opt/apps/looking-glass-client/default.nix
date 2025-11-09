{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.apps.looking-glass-client;
  user = config.nerv.opt.user;
in
{
  options.nerv.opt.apps.looking-glass-client = with types; {
    enable =
      mkBoolOpt false "Whether or not to enable the Looking Glass client.";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ looking-glass-client ];

    environment.etc."looking-glass-client.ini" = {
      user = "+${toString config.users.users.${user.name}.uid}";
      source = ./client.ini;
    };
  };
}
