{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.apps.virtualbox;
in
{
  options.nerv.opt.apps.virtualbox = with types; {
    enable = mkBoolOpt false "Whether or not to enable Virtualbox.";
  };

  config = mkIf cfg.enable {
    virtualisation.virtualbox.host = {
      enable = true;
      enableExtensionPack = true;
    };

    nerv.opt.user.extraGroups = [ "vboxusers" ];
  };
}
