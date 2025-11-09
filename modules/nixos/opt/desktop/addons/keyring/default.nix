{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.opt.desktop.addons.keyring;
in
{
  options.nerv.opt.desktop.addons.keyring = with types; {
    enable = mkBoolOpt false "Whether to enable a keyring service for managing secrets.";
    backend = mkOption {
      type = enum [ "pass" "gnome" ];
      default = "pass";
      description = "Which keyring backend to use: 'pass' (uses pass-secret-service) or 'gnome' (uses gnome-keyring).";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      assertions = [
        {
          assertion = cfg.enable -> (cfg.backend == "pass" || cfg.backend == "gnome");
          message = "Desktop keyring addon requires a valid backend ('pass' or 'gnome') when enabled.";
        }
      ];
    }

    (mkIf (cfg.backend == "pass") {
      services.passSecretService.enable = true;
      nerv.opt.security.pass = on;
      programs.seahorse.enable = true;
    })

    # TODO: why I cannot unlock keyring :( ?
    (mkIf (cfg.backend == "gnome") {
      services.gnome.gnome-keyring.enable = true;
      security.pam.services.sddm.enableGnomeKeyring = true;
      programs.seahorse.enable = true;
      nerv.opt.persist.state.homeDirectories = [
        { directory = ".local/share/keyrings"; mode = "0700"; }
        { directory = ".pki/nssdb"; mode = "0700";  } # gnome keyring?
      ];
    })
  ]);
}
