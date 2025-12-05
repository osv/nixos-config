{ options, config, lib, pkgs, ... }:

with lib.nerv;
let
  inherit (lib) mapAttrsToList strings;

  # Import shared themes
  themeDefinitions = import ../themes.nix { inherit lib; };
  
  # Map themes to keybindings
  themes = {
    "C-1" = themeDefinitions.themes.dracula;
    "C-2" = themeDefinitions.themes.light;
    "C-3" = themeDefinitions.themes.gruvbox;
    "C-4" = themeDefinitions.themes.dirty;
    "C-5" = themeDefinitions.themes.twentyTwo;
    "C-6" = themeDefinitions.themes.rosePineDawn;
  };

  # pick "Dracula" as the default theme
  defaultTheme = themes."C-1";

  custom-rxvt-unicode = pkgs.rxvt-unicode.override {
    configure = { availablePlugins, ... }: {
      plugins = with availablePlugins; [ autocomplete-all-the-things ];
    };
  };
  urxvt = "${custom-rxvt-unicode}/bin/urxvt";
  urxvtd = "${custom-rxvt-unicode}/bin/urxvtd";
  urxvtc = "${custom-rxvt-unicode}/bin/urxvtc";
  ld-rxvt-preload =
    "LD_PRELOAD=${pkgs.nerv.wcwidth-icons}/lib/libwcwidth-icons.so";
  urxvtNerdIconSupport = pkgs.writeShellScriptBin "urxvt" ''
    ${ld-rxvt-preload} ${urxvt} "$@"
  '';
  urxvtcNerdIconSupport = pkgs.writeShellScriptBin "urxvtc" ''
    ${ld-rxvt-preload} ${urxvtc} "$@"
    if [ $? -eq 2 ]; then
      echo "Starting urxvtd..."
      ${urxvtd} -q -o -f
      echo "Done."
      ${ld-rxvt-preload} ${urxvtc} "$@"
    fi
  '';

  # read in your static Xresources fragment
  baseXres = builtins.readFile ./Xresources;
in {

  options.nerv.opt.desktop.addons.term.urxvt = with lib.types; {
    enable = mkBoolOpt false "Enable URxvt with dynamic theming";
  };

  config = lib.mkIf config.nerv.opt.desktop.addons.term.urxvt.enable {
    environment.systemPackages = [
      urxvtNerdIconSupport
      urxvtcNerdIconSupport
      custom-rxvt-unicode
    ];

    nerv.opt.desktop.addons.xresources.extraConfigs = [
      baseXres
      ''

      ! Default theme for urxvt
      ''
      (urxvtHelper.defaultTheme defaultTheme)
      ''

      ! ---------------------------------------------------------------------
      ! You can switch at runtime with:
      !   urxvt -name Dracula
      !   urxvt -name Dirty
      ! ---------------------------------------------------------------------
      ''
      (urxvtHelper.xresourcesUrxvtThemes themes)
      ''

      ! Change theme by hot keys
      ''
      (urxvtHelper.xresourcesChangeThemesCommands themes)

    ];
  };
}
