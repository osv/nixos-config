{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.desktop.addons.xresources;
  substitutedConfig = pkgs.replaceVars ./Xresources {
    terminal = config.nerv.opt.desktop.addons.term.defaultTerminal;
  };
in {
  options.nerv.opt.desktop.addons.xresources = with types; {
    enable = mkBoolOpt false "Create .Xresources";
    extraConfigs = mkOpt (listOf str) [ ] "Extra config added to .Xresources.";
  };

  config = mkIf cfg.enable {
    nerv.home = {
      file.".Xresources".text = builtins.readFile (substitutedConfig)
        + (strings.concatStringsSep "\n" cfg.extraConfigs);

      activation.xrdb = {
        after = [ "linkGeneration" ];
        before = [ ];
        data = "DISPLAY=:0 ${pkgs.xorg.xrdb}/bin/xrdb ~/.Xresources || exit 0";
      };
    };
  };
}
