{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.opt.desktop.addons.qt-theme;
in
{
  options.nerv.opt.desktop.addons.qt-theme = with types; {
    enable = mkBoolOpt false "Whether to customize Qt/Plasma and apply themes.";

    theme = {
      name = mkOpt str "Catppuccin-Frappe-Blue"
        "The name of the Qt/Plasma global theme to apply.";
      pkg = mkOpt package pkgs.catppuccin-kde "The package to use for the Qt/Plasma theme.";
    };

    colorScheme = {
      name = mkOpt str "CatppuccinFrappeBlue"
        "The name of the KDE color scheme to apply.";
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      cfg.theme.pkg  # Qt/Plasma theme package
    ];

    # Apply KDE/Qt theme via plasma-hack
    nerv.opt.desktop.addons.plasma-hack.kconfigs = [
      {
        kdeglobals = {
          KDE.LookAndFeelPackage = "org.kde.${cfg.theme.name}";
          General.ColorScheme = cfg.colorScheme.name;
          General.Name = cfg.colorScheme.name;
          General.widgetStyle = "Breeze";
        };
      }
    ];
  };
}
