{ options, config, lib, pkgs, inputs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.apps.zoom;
in
{
  options.nerv.opt.apps.zoom = with types; {
    enable = mkBoolOpt false "Whether or not to enable Zoom.";
  };

  config = mkIf cfg.enable {
    nerv.opt.persist.state.homeFiles = [
      ".config/zoom.conf"
      ".config/zoomus.conf"
    ];

    nerv.opt.persist.state.homeDirectories = [
      ".zoom"
    ];

    environment.systemPackages = with pkgs; [
      zoom-us
    ];
  };
}
