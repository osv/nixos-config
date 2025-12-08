{ options, config, lib, pkgs, inputs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.desktop.addons.wallpaper;

  # Default wallpaper path
  defaultWallpaper = "${inputs.wallpapers-new}/pic/fish.jpg";

  # Script to set wallpaper using feh
  setWallpaperScript = pkgs.writeShellScriptBin "my-set-wallpaper" ''
    WALLPAPER="''${1:-${toString cfg.image}}"

    if [ -f "$WALLPAPER" ]; then
      ${pkgs.feh}/bin/feh ${cfg.mode} "$WALLPAPER"
    else
      echo "Wallpaper not found: $WALLPAPER" >&2
      exit 1
    fi
  '';
in
{
  options.nerv.opt.desktop.addons.wallpaper = with types; {
    enable = mkBoolOpt false "Whether to enable desktop wallpaper management.";
    image = mkOpt (either str path) defaultWallpaper "Path to wallpaper image (string or path).";
    mode = mkOpt str "--bg-fill" "feh mode for wallpaper (--bg-fill, --bg-scale, --bg-center, --bg-tile).";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.feh
      setWallpaperScript
    ];
  };
}
