{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.apps.rtsa-suite;
in
{
  options.nerv.opt.apps.rtsa-suite = {
    enable = mkBoolOpt false "Whether or not to enable aaronia rtsa-suite (Spectrum analyzer software for SPECTRAN V6).";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.nerv.rtsa-suite ];

    nerv.opt.persist = {
      state.homeDirectories = [
        ".config/Aaronia AG"
        ".local/share/Aaronia AG/Aaronia RTSA-Suite PRO/udata"
      ];
      # Decoders/keys/maps downloaded by the built-in package manager (gdata).
      derivative.homeDirectories = [
        ".local/share/Aaronia AG/Aaronia RTSA-Suite PRO/gdata"
      ];
    };

    services.udev.extraRules = builtins.readFile ./72-aaronia.rules;
    # Raised USB buffer limit for high-rate IQ streaming.
    # For long IQ captures Aaronia also recommends cma=256M in
    # kernelParams (reserves memory — disabled on the laptop).
    # Plain list definitions merge with the host's kernel parameters.
    # mkDefault would discard this entire list when the host defines its own.
    boot.kernelParams = [ "usbcore.usbfs_memory_mb=256" ];
    boot.kernelModules = [ "usbtmc" ];
  };
}
