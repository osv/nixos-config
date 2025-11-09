{ options, config, lib, pkgs, inputs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.desktop.gnome;
in
{
  options.nerv.opt.desktop.gnome = with types; {
    enable =
      mkBoolOpt false "Whether or not to use Gnome as the desktop environment.";
    wallpaper = {
      light = mkOpt (oneOf [ str package ]) "${inputs.wallpapers-new}/fish.jpg" "The light wallpaper to use.";
      dark = mkOpt (oneOf [ str package ]) "${inputs.wallpapers-new}/misty-forest.jpg" "The dark wallpaper to use.";
    };
    color-scheme = mkOpt (enum [ "light" "dark" ]) "dark" "The color scheme to use.";
  };

  config = mkIf cfg.enable {
    nerv.opt.system.xkb.enable = true;
    nerv.opt.desktop.addons = {
      gtk = on;
      wallpapers = on;
      electron-support = on;
    };

    environment.systemPackages = with pkgs; [
      (hiPrio nerv.xdg-open-with-portal)
      wl-clipboard
      gnome.gnome-tweaks
      gnomeExtensions.appindicator
      gnomeExtensions.big-avatar
      gnomeExtensions.no-overview
      gnomeExtensions.wireless-hid
      gnomeExtensions.emoji-selector
      gnomeExtensions.clear-top-bar
      gnomeExtensions.dash-to-dock
      gnomeExtensions.blur-my-shell
      gnomeExtensions.extension-list
      gnomeExtensions.just-perfection
      gnomeExtensions.transparent-top-bar
      gnomeExtensions.gsconnect
      gnomeExtensions.gtile
      gnomeExtensions.audio-output-switcher
    ];

    environment.gnome.excludePackages = with pkgs.gnome; [
      pkgs.gnome-tour
      epiphany
      geary
      gnome-font-viewer
      gnome-system-monitor
      gnome-maps
    ];

    systemd.services.nerv-user-icon = {
      before = [ "display-manager.service" ];
      wantedBy = [ "display-manager.service" ];

      serviceConfig = {
        Type = "simple";
        User = "root";
        Group = "root";
      };

      script = ''
        config_file=/var/lib/AccountsService/users/${config.nerv.opt.user.name}
        icon_file=/run/current-system/sw/share/nerv-icons/user/${config.nerv.opt.user.name}/${config.nerv.opt.user.icon.fileName}

        if ! [ -d "$(dirname "$config_file")"]; then
          mkdir -p "$(dirname "$config_file")"
        fi

        if ! [ -f "$config_file" ]; then
          echo "[User]
          Session=gnome
          SystemAccount=false
          Icon=$icon_file" > "$config_file"
        else
          icon_config=$(sed -E -n -e "/Icon=.*/p" $config_file)

          if [[ "$icon_config" == "" ]]; then
            echo "Icon=$icon_file" >> $config_file
          else
            sed -E -i -e "s#^Icon=.*$#Icon=$icon_file#" $config_file
          fi
        fi
      '';
    };

    # Required for app indicators
    services.udev.packages = with pkgs; [ gnome3.gnome-settings-daemon ];

    nerv.home.extraOptions = {
      dconf.settings =
        let
          user = config.users.users.${config.nerv.opt.user.name};
          get-wallpaper = wallpaper:
            if lib.isDerivation wallpaper then
              builtins.toString wallpaper
            else
              wallpaper;
        in
        {
          "org/gnome/desktop/background" = {
            "picture-uri" = get-wallpaper cfg.wallpaper.light;
            "picture-uri-dark" = get-wallpaper cfg.wallpaper.dark;
          };
          "org/gnome/desktop/screensaver" = {
            "picture-uri" = get-wallpaper cfg.wallpaper.light;
            "picture-uri-dark" = get-wallpaper cfg.wallpaper.dark;
          };
          "org/gnome/desktop/interface" = {
            "color-scheme" = if cfg.color-scheme == "light" then "default" else "prefer-dark";
          };
        };
    };

    programs.kdeconnect = {
      enable = true;
      package = pkgs.gnomeExtensions.gsconnect;
    };

    # Open firewall for samba connections to work.
    networking.firewall.extraCommands =
      "iptables -t raw -A OUTPUT -p udp -m udp --dport 137 -j CT --helper netbios-ns";
  };

}
