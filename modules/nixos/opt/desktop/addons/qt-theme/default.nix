{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.desktop.addons.qt-theme;

  # Helper function to capitalize first letter
  capitalize = str:
    let
      first = substring 0 1 str;
      rest = substring 1 (-1) str;
    in
    toUpper first + rest;

  # Theme name for KDE (CamelCase)
  kdeThemeName = "Catppuccin-${capitalize cfg.flavor}-${capitalize cfg.accent}";
  kdeColorScheme = "Catppuccin${capitalize cfg.flavor}${capitalize cfg.accent}";

  # Theme name for qt6ct (lowercase)
  qtThemeName = "catppuccin-${cfg.flavor}-${cfg.accent}";

  # Convert nested attrs to kwriteconfig6 commands (like plasma-hack)
  qt6ctAttrToCommands = path: value:
    let
      file = "~/.config/qt6ct/qt6ct.conf";
      group = lib.concatMapStringsSep "/" (s: ''"${s}"'') (lib.init path);
      key = lib.last path;
      valueType = if value == true then "bool"
                else if value == false then "bool"
                else if builtins.isInt value then "num"
                else "str";
      valueStr = if value == true then "true"
                else if value == false then "false"
                else toString value;
    in
    "${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 --file ${file} --group ${group} --key ${key} --type ${valueType} ${valueStr}";

  qt6ctFlattenAttrs = attrs: pathSoFar:
    lib.lists.flatten (lib.attrsets.mapAttrsToList
      (name: value:
        if builtins.isAttrs value
        then qt6ctFlattenAttrs value (pathSoFar ++ [ name ])
        else qt6ctAttrToCommands (pathSoFar ++ [ name ]) value
      )
    attrs);

  applyQt6ctConfig = attrs:
    lib.strings.concatStringsSep "\n" (qt6ctFlattenAttrs attrs [ ]);
in
{
  options.nerv.opt.desktop.addons.qt-theme = with types; {
    enable = mkBoolOpt false "Whether to customize Qt applications (Qt5/Qt6) with Catppuccin theme. Works with both XMonad and KDE Plasma.";

    flavor = mkOption {
      type = enum [ "latte" "frappe" "macchiato" "mocha" ];
      default = "frappe";
      description = "Catppuccin flavor to use.";
    };

    accent = mkOption {
      type = enum [ "blue" "red" "pink" "orange" "yellow" "green" "teal" "sky" "sapphire" "lavender" "flamingo" "mauve" "maroon" "peach" "rosewater" ];
      default = "blue";
      description = "Catppuccin accent color.";
    };

    qt6ct = mkOption {
      type = with types; listOf attrs;
      default = [ ];
      description = "List of qt6ct configurations. Each module adds its own settings which are merged.";
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      qt6Packages.qt6ct       # Qt6 configuration tool (for XMonad)
      catppuccin-qt5ct        # Catppuccin theme for qt5ct/qt6ct
      kdePackages.breeze-icons # Breeze icon theme
      kdePackages.breeze       # Breeze Qt style/theme
    ];

    # Make Qt applications use qt6ct for theming (works in XMonad)
    # In KDE Plasma, this is ignored (Plasma has priority)
    environment.sessionVariables = {
      QT_QPA_PLATFORMTHEME = "qt6ct";
    };

    # Copy catppuccin color schemes to qt6ct's expected location
    systemd.user.tmpfiles.rules = [
      "L+ /home/osv/.local/share/qt6ct/colors - - - - ${pkgs.catppuccin-qt5ct}/share/qt6ct/colors"
    ];

    # Configure KDE color scheme (works with KDE Plasma and Qt apps that read kdeglobals)
    nerv.opt.desktop.addons.plasma-hack.kconfigs = [
      {
        kdeglobals = {
          # KDE Global Theme (for Plasma)
          KDE.LookAndFeelPackage = "org.kde.${kdeThemeName}";

          # Color Scheme (for Plasma and Qt apps)
          General.ColorScheme = kdeColorScheme;
          General.Name = kdeColorScheme;

          # Widget style
          General.widgetStyle = "Breeze";
        };
      }
    ];

    # Add qt6ct theme settings to the list (allows other modules to add their own qt6ct configs)
    nerv.opt.desktop.addons.qt-theme.qt6ct = [
      {
        Appearance = {
          color_scheme_path = "${pkgs.catppuccin-qt5ct}/share/qt6ct/colors/${qtThemeName}.conf";
          custom_palette = true;
          icon_theme = "breeze";
          style = "Windows";  # User-selected style (Windows, Fusion, etc.)
          font = "Noto Sans, 12, -1, 5, 400, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, Regular, 0, 0";
        };
        Fonts = {
          general = "Noto Sans, 12, -1, 5, 400, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, Regular, 0, 0";
          fixed = "Monospace, 12, -1, 5, 400, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, Regular, 0, 0";
        };
        Palette = {
          color_scheme_path = "${pkgs.catppuccin-qt5ct}/share/qt6ct/colors/${qtThemeName}.conf";
          custom_color_scheme = true;
        };
      }
    ];

    # Configure qt6ct to apply all settings via systemd service (patches existing config)
    nerv.home.extraOptions.systemd.user.services.qt6ct-setup = lib.mkIf (cfg.qt6ct != [ ]) {
      Unit = {
        Description = "Apply qt6ct configuration from Nix";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = pkgs.writeShellScript "qt6ct-setup" ''
          # Ensure qt6ct config directory exists
          mkdir -p ~/.config/qt6ct

          # Apply all qt6ct settings using kwriteconfig6 (patches existing config)
          # This flattens the nested attrs and applies each setting
          ${applyQt6ctConfig (lib.foldr lib.recursiveUpdate { } cfg.qt6ct)}
        '';
      };
      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
}
