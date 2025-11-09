{ config, lib, pkgs, ... }:

with lib;
with lib.nerv;

stdModules { inherit config lib pkgs; } [
  [
    "nerv.opt.desktop.addons.boomer"
    "x11 zoom app (Boomer)"
    (with pkgs; [ boomer ])
    {}
    {
      nerv.home.configFile."boomer/config".text = ''
        min_scale = 1
        scroll_speed = 1.2
        drag_friction = 6.0
        scale_friction = 4.0
      '';
    }
  ]

  [
    "nerv.opt.desktop.addons.notify"
    "notification daemon. Good to use with WM like xmonad"
    (with pkgs; [ libnotify ])
    {}
    {
      nerv.home.extraOptions.services.dunst = on;
    }
  ]

  [
    "nerv.opt.desktop.addons.swappy"
    "Swappy in the desktop environment"
    (with pkgs; [ swappy ])
    {}
    {
      nerv.home.configFile."swappy/config".source = ./addons/swappy/config;
      nerv.home.file."Pictures/screenshots/.keep".text = "";
    }
  ]

  [
    "nerv.opt.desktop.addons.waybar"
    "Waybar in the desktop environment"
    (with pkgs; [ waybar ])
    {}
    {
      nerv.home.configFile."waybar/config".source = ./addons/waybar/config;
      nerv.home.configFile."waybar/style.css".source = ./addons/waybar/style.css;
    }
  ]

  [
    "nerv.opt.desktop.addons.wofi"
    "the Wofi in the desktop environment"
    (with pkgs; [ wofi wofi-emoji ])
    {}
    {
      nerv.home.configFile."wofi/config".source = ./addons/wofi/config;
      nerv.home.configFile."wofi/style.css".source = ./addons/wofi/style.css;
    }
  ]

  [
    "nerv.opt.desktop.addons.mako"
    "Mako in Sway"
    (with pkgs; [ mako libnotify ])
    {}
    {
      systemd.user.services.mako = {
        description = "Mako notification daemon";
        wantedBy = [ "graphical-session.target" ];
        partOf = [ "graphical-session.target" ];
        after = [ "graphical-session.target" ];
        serviceConfig = {
          Type = "dbus";
          BusName = "org.freedesktop.Notifications";

          ExecCondition = ''
            $${pkgs.bash}/bin/bash -c '[ -n "$$WAYLAND_DISPLAY" ]'
          '';

          ExecStart = ''
            $${pkgs.mako}/bin/mako
          '';

          ExecReload = ''
            $${pkgs.mako}/bin/makoctl reload
          '';

          Restart = "on-failure";
          RestartSec = 1;
          TimeoutStopSec = 10;
        };
      };

      nerv.home.configFile."mako/config".source = ./addons/mako/config;
    }
  ]

  [
    "nerv.opt.desktop.displayManager.sddm"
    "SDDM as the desktop manager"
    []
    {}
    {
      services.displayManager.sddm.enable = true;
      services.xserver.enable = true;
    }
  ]
]
