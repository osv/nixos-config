{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.desktop.addons.term;
  yamlFormat = pkgs.formats.yaml { };

  my-test-terminal = pkgs.writeScriptBin "my-test-terminal"
    (builtins.readFile ./my-test-terminal);

  alacrittyBaseSettings = {
    window = {
      padding = {
        x = 10;
        y = 10;
      };
    };
    font = {
      normal = {
        family = "JetBrains Mono";
        Style = "Bold";
      };
      bold = {
        family = "JetBrains Mono";
        style = "Bold";
      };
      italic = {
        family = "JetBrains Mono";
        style = "Italic";
      };
      size = 11.0;
    };
    cursor = {
      style = {
        shape = "Underline";
        blinking = "On";
      };
    };
    key_bindings = [
      {
        key = "F";
        mods = "Control|Shift";
        mode = "~Search";
        action = "SearchForward";
      }
      {
        key = "Key0";
        mods = "Control";
        action = "ResetFontSize";
      }
      {
        key = "Plus";
        mods = "Control";
        action = "IncreaseFontSize";
      }
      {
        key = "Minus";
        mods = "Control";
        action = "DecreaseFontSize";
      }
    ];
  };
in {
  options.nerv.opt.desktop.addons.term = with types; {
    enable = mkBoolOpt false "Default terminal for different window managers.";
    defaultTerminal =
      mkOpt (nullOr str) null "The default terminal to use in system.";
    alacritty = {
      enable = mkBoolOpt false "Enable alacritty";
      settings = mkOpt yamlFormat.type alacrittyBaseSettings
        "Base configuration for alacritty";
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      assertions = [
        {
          assertion = cfg.alacritty.enable || cfg.kitty.enable || cfg.urxvt.enable || cfg.wezterm.enable;
          message =
            "desktop.addons.term: You should enable atleast one terminal.";
        }
        {
          assertion = !(builtins.isNull cfg.defaultTerminal);
          message = "desktop.addons.term: You should set defaultTerminal";
        }
      ];
    })

    (mkIf (cfg.enable) {
      environment.sessionVariables.TERMINAL = cfg.defaultTerminal;
      environment.systemPackages = [ my-test-terminal ];
    })

    (mkIf (cfg.enable && cfg.alacritty.enable) {
      environment.systemPackages = [ pkgs.alacritty ];
      nerv.home.configFile."alacritty/alacritty.yml".text =
        builtins.toJSON cfg.alacritty.settings;
    })
  ];
}
