{ config, lib, pkgs, ... }:
with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.persist.common;
in
{
  options.nerv.opt.persist.common = {
    enable = mkBoolOpt true "Whether or not to enable common cache and state directories like .cache, machine-id.";
  };

  config = mkIf cfg.enable {
    nerv.opt.persist = {

      cache = {
        directories = [ "/var/cache" ];
        homeDirectories = [ ".cache" ".local/share/Trash" ];
      };

      state = {
        files = [
          "/etc/machine-id"
          "/etc/nix/id_rsa"
        ];
        directories = [
          "/var/log"
          "/var/lib/bluetooth"
          "/var/lib/nixos"
          # "/root"
          # "/var/lib/systemd/coredump"
          {
            directory = "/var/lib/colord";
            user = "colord";
            group = "colord";
            mode = "u=rwx,g=rx,o=";
          }
        ];
        homeDirectories = [
          "stash" # skip backup, smtg like tmp but persist
          ".bundler" # ruby bundler cache
          ".codex"
          ".gemini"
          ".local/share/uv" # python uv package manager
        ];
        homeFiles = [
          ".config/uv/settings.toml"
        ];
      };
    };
  };
}
