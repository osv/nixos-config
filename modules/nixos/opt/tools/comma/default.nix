{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.tools.comma;
in
{
  options.nerv.opt.tools.comma = with types; {
    enable = mkBoolOpt false "Whether or not to enable comma. Comma runs software without installing it. Try to type `, cowsay neato`";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      comma
      nerv.nix-update-index
    ];

    nerv.home.extraOptions = { programs.nix-index.enable = true; };
  };
}
