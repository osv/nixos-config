{ options, config, lib, pkgs, inputs, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.opt.apps.viber;
in {
  options.nerv.opt.apps.viber = with types; {
    enable = mkBoolOpt false "Whether or not to enable Viber.";
  };

  config = mkIf cfg.enable {
    nerv.opt.persist.state.homeDirectories = [ ".ViberPC" ];

    networking.firewall.allowedTCPPorts = [ 5242 4244 5243 7985 ];
    networking.firewall.allowedUDPPorts = [ 5242 4244 5243 7985 ];

    environment.systemPackages = with pkgs; [ viber ];
  };
}
