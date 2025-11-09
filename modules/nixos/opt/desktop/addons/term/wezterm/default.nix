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
    material = themeDefinitions.themes.material;
  };

  # Generate theme files using the wezterm helper
  themeFiles = weztermHelper.generateThemeFiles themes;
in {
  options.nerv.opt.desktop.addons.term.wezterm = with types; {
    enable = mkBoolOpt false "Enable WezTerm terminal emulator";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.wezterm ];
    
    # Write WezTerm configuration and theme files
    nerv.home.configFile = {
      "wezterm/wezterm.lua".source = ./config.lua;
    } // themeFiles;
  };
}
