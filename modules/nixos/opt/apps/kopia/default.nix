{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.apps.kopia;
  keyring = config.nerv.opt.desktop.addons.keyring;
in
{
  options.nerv.opt.apps.kopia = {
    enable = mkBoolOpt false "Whether or not to enable Kopia backups through rclone.";
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = keyring.enable && keyring.backend == "pass";
        message = "Kopia requires nerv.opt.desktop.addons.keyring with the pass backend.";
      }
    ];

    environment = {
      systemPackages = with pkgs; [
        kopia
        kopia-ui
        rclone
      ];
      sessionVariables.KOPIA_USE_KEYRING = "true";
    };

    nerv.opt.persist.state.homeDirectories = [
      {
        directory = ".config/kopia";
        mode = "0700";
      }
      {
        directory = ".config/rclone";
        mode = "0700";
      }
    ];
  };
}
