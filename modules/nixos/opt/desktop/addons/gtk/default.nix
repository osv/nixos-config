{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.opt.desktop.addons.gtk;
in
{
  options.nerv.opt.desktop.addons.gtk = with types; {
    enable = mkBoolOpt false "Whether to customize GTK and apply themes.";
    theme = {
      name = mkOpt str "catppuccin-frappe-blue-standard"
        "The name of the GTK theme to apply.";
      pkg = mkOpt package pkgs.catppuccin-gtk "The package to use for the theme.";
    };
    cursor = {
      name = mkOpt str "Bibata-Modern-Ice"
        "The name of the cursor theme to apply.";
      pkg = mkOpt package pkgs.bibata-cursors "The package to use for the cursor theme.";
    };
    icon = {
      name = mkOpt str "Papirus"
        "The name of the icon theme to apply.";
      pkg = mkOpt package pkgs.papirus-icon-theme "The package to use for the icon theme.";
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      cfg.theme.pkg  # GTK theme package
      cfg.cursor.pkg # Cursor theme package
      cfg.icon.pkg   # Icon theme package
    ];

    environment.sessionVariables = {
      XCURSOR_THEME = cfg.cursor.name;
    };

    nerv.home.extraOptions = {
      gtk = {
        enable = true;

        theme = {
          name = cfg.theme.name;
          package = cfg.theme.pkg;
        };

        # Explicitly set gtk4.theme to null to silence 26.05 warning (new behavior)
        gtk4.theme = null;

        cursorTheme = {
          name = cfg.cursor.name;
          package = cfg.cursor.pkg;
        };

        iconTheme = {
          name = cfg.icon.name;
          package = cfg.icon.pkg;
        };
      };

      # Tell portal-aware apps (flatpak, Firefox "Default" theme, libadwaita, etc.)
      # to render dark. The gtk portal (org.freedesktop.impl.portal.Settings) reads
      # this gsetting and propagates it; without it the system reports "light".
      dconf.settings."org/gnome/desktop/interface".color-scheme = "prefer-dark";
    };
  };
}
