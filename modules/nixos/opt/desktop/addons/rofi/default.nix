{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.opt.desktop.addons.rofi;
in {
  options.nerv.opt.desktop.addons.rofi = with types; {
    enable =
      mkBoolOpt false "Whether to enable Rofi in the desktop environment.";
  };

  config = mkIf cfg.enable {
    # environment.systemPackages = with pkgs; [ rofi ];
    nerv.home.extraOptions.programs = {
      rofi = {
        enable = true;
        cycle = true;
        # font = "${custom.font.name} ${toString custom.font.size}";
        location = "center";
        terminal = config.nerv.opt.desktop.addons.term.defaultTerminal;
        # theme = ./kenran.rasi;
        extraConfig = { modi = "run,drun,ssh,window"; };
      };
    };
    # nerv.home.configFile."rofi/config.rasi".source = ./config.rasi;
  };
}
