{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.suites.common-slim;
in
{
  options.nerv.suites.common-slim = with types; {
    enable = mkBoolOpt false "Whether or not to enable common-slim configuration.";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.nerv.list-iommu
    ];

    nerv = {
      opt.nix = on;

      opt.cli-apps = { };

      opt.tools = {
        git = on;
        fup-repl = on;
        comma = on;
        bottom = on;
        direnv = on;
      };

      opt.hardware = {
        networking = on;
      };

      opt.services = {
        openssh = on;
        tailscale = on;
      };

      opt.security = {
        doas = on;
      };

      opt.system = {
        boot = on;
        fonts = on;
        locale = on;
        time = on;
        xkb = on;
      };
    };
  };
}
