{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.tools.steganography;
  privateBackupScript = config.sops.secrets.my-secrets-backup;
in
{
  options.nerv.opt.tools.steganography = with types; {
    enable = mkBoolOpt false "Whether or not to enable encrypted steganography backup and restore tools.";
  };

  config = mkIf cfg.enable {
    sops.secrets.my-secrets-backup = {
      sopsFile = ../../../../../secrets/my-secrets-backup.bash.binary;
      format = "binary";
      owner = config.nerv.opt.user.name;
      mode = "0400";
    };

    environment.systemPackages = [
      (pkgs.writeShellApplication {
        name = "my-secrets-restore";
        runtimeInputs = with pkgs; [
          p7zip
          steghide
          coreutils
          findutils
          gnugrep
        ];
        text = builtins.readFile ./my-secrets-restore.bash;
      })

      (pkgs.writeShellApplication {
        name = "my-secrets-backup";
        runtimeInputs = with pkgs; [
          git
          pass
          gnupg
          p7zip
          steghide
          coreutils
        ];
        text = ''
          if [ "$#" -ne 0 ]; then
            echo "Usage: my-secrets-backup" >&2
            exit 1
          fi
          exec ${lib.getExe pkgs.bash} ${privateBackupScript.path}
        '';
      })
    ];
  };
}
