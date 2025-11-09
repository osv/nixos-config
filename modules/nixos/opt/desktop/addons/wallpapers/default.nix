{ options, config, pkgs, lib, inputs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.desktop.addons.wallpapers;
  
  # Get all image files from wallpapers-new/pic directory
  picDir = "${inputs.wallpapers-new}/pic";
  allFiles = builtins.readDir picDir;
  
  # Filter for image extensions
  imageExtensions = [ "jpg" "jpeg" "png" "gif" "bmp" "webp" "svg" ];
  isImageFile = fileName: 
    let
      extension = lib.strings.toLower (lib.lists.last (lib.strings.splitString "." fileName));
    in
    builtins.elem extension imageExtensions;
  
  # Get only image files
  imageFiles = lib.attrNames (lib.filterAttrs (name: type: type == "regular" && isImageFile name) allFiles);
  
  # Create home file mappings
  wallpaperFiles = lib.foldl
    (acc: fileName:
      acc // {
        "Pictures/wallpapers/${fileName}".source = "${picDir}/${fileName}";
      })
    { }
    imageFiles;
in
{
  options.nerv.opt.desktop.addons.wallpapers = with types; {
    enable = mkBoolOpt false
      "Whether or not to add wallpapers from wallpapers-new repo to ~/Pictures/wallpapers.";
  };

  config = mkIf cfg.enable {
    nerv.home.file = wallpaperFiles;
  };
}
