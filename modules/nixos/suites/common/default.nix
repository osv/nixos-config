{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.suites.common;
in {
  options.nerv.suites.common = with types; {
    enable = mkBoolOpt false "Whether or not to enable common configuration.";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.nerv.list-iommu ];

    nerv = {
      opt.nix = on;

      opt.cli-apps = { };

      opt.common = {
        checks = on;
      };

      opt.tools = {
        git = on;
        misc = on;
        fup-repl = on;
        comma = on;
        bottom = on;
        steganography = on;
      };

      opt.hardware = {
        audio = on;
        networking = on // {
          blockShittySites = no;
        };
      };

      opt.services = {
        printing = off;
        openssh = on;
        tailscale = off;
      };

      opt.security = {
        dns = on;
        gpg = on;
        doas = on;
        keyring = off;
      };

      opt.system = {
        boot = on;
        fonts = on;
        locale = on;
        time = on;
        xkb = on // {
          xkbPerWindow = yes;
        };
      };
    };
  };
}
