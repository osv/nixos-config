{ inputs, options, config, pkgs, lib, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.opt.hardware.networking;
in
{
  options.nerv.opt.hardware.networking = with types; {
    enable = mkBoolOpt false "Whether or not to enable networking support";
    hosts = mkOpt attrs { }
      "An attribute set to merge with <option>networking.hosts</option>";

    blockShittySites = mkBoolOpt false "Whether or not to enable networking support";
  };

  imports = [
    inputs.stevenBlackHosts.nixosModule
  ];

  config = mkIf cfg.enable {
    nerv.opt.user.extraGroups = [ "networkmanager" ];

    networking = {
      hosts = {
        "127.0.0.1" = [ "local.test" ] ++ (cfg.hosts."127.0.0.1" or [ ]);
      } // cfg.hosts;

      extraHosts =
        ''
          127.0.0.1 console.app
          127.0.0.1 console.localhost
          127.0.0.1 reliz.app
          127.0.0.1 reliz.localhost
          127.0.0.1 inmotion.app
          127.0.0.1 inmotion.localhost
          127.0.0.1 smaato.app
          127.0.0.1 smaato.localhost
          128.0.0.1 dentsu.localhost
        '';
      stevenBlackHosts = mkIf cfg.blockShittySites {
        enable = true;
        blockGambling = yes;
      };

      networkmanager = {
        enable = true;
        dhcp = "internal";
      };
    };

    nerv.opt.persist.state.directories = [ "/etc/NetworkManager/system-connections" ];

    # Fixes an issue that normally causes nixos-rebuild to fail.
    # https://github.com/NixOS/nixpkgs/issues/180175
    systemd.services.NetworkManager-wait-online.enable = false;
  };
}
