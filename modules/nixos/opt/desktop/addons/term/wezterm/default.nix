{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.desktop.addons.term.wezterm;

  # Import shared themes
  themeDefinitions = import ../themes.nix { inherit lib; };

  # Map themes to match urxvt keybindings
  themes = {
    dracula = themeDefinitions.themes.dracula;
    light = themeDefinitions.themes.light;
    gruvbox = themeDefinitions.themes.gruvbox;
    dirty = themeDefinitions.themes.dirty;
    twentyTwo = themeDefinitions.themes.twentyTwo;
    rosePineDawn = themeDefinitions.themes.rosePineDawn;
  };

  # Generate theme files using the wezterm helper
  themeFiles = weztermHelper.generateThemeFiles themes;
in {
  options.nerv.opt.desktop.addons.term.wezterm = with types; {
    enable = mkBoolOpt false "Enable WezTerm terminal emulator";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      wezterm

      ## Test config script
      (mkTestConfigScript pkgs {
        name = "my-test-wezterm-config";
        appName = "WezTerm";
        sourcePath = "modules/nixos/opt/desktop/addons/term/wezterm/config.lua";
        targetPath = ".config/wezterm";
        files = [ "wezterm.lua" ];
        reloadCmd = "Restart WezTerm";
      })
    ];

    # Write WezTerm configuration and theme files
    nerv.home.configFile = {
      "wezterm/wezterm.lua".source = ./config.lua;
    } // themeFiles;
  };
}
