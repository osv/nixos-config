{ lib, pkgs, config, virtual, ... }:

let
  inherit (lib) mkIf mkMerge mkEnableOption optional;
  inherit (lib.nerv) mkOpt;

  cfg = config.nerv.opt.security.vpn;
in {
  options.nerv.opt.security.vpn = with lib.types; {
    enable = mkEnableOption "Enables VPN";
    kzVPN = mkEnableOption "Enables KZ VPN";
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.kzVPN) {
      services.openvpn = {
        servers = {
          kz-development = {
            config = "config ${config.sops.secrets.vpn-kzd.path}";
            autoStart = false;
            # updateResolvConf = true;
          };

          kz-production = {
            config = "config ${config.sops.secrets.vpn-kzp.path}";
            autoStart = false;
            # updateResolvConf = true;
          };
        };
      };
    })
  ];
}
