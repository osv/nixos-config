{ config, lib, pkgs, ... }:

with lib;
with lib.nerv;

stdModules { inherit config lib pkgs; } [
  [
    "nerv.opt.apps.ardour"
    "Ardour"
    (with pkgs; [ ardour ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.blender"
    "Blender"
    (with pkgs; [ blender ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.bottles"
    "Bottles"
    (with pkgs; [ bottles ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.cadence"
    "Cadence"
    (with pkgs; [ cadence ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.chromium"
    "Chromium"
    (with pkgs; [ chromium ])
    { state.homeDirectories = [ ".config/chromium" ]; }
    {}
  ]

  [
    "nerv.opt.apps.doukutsu-rs"
    "doukutsu-rs"
    (with pkgs; [ nerv.doukutsu-rs ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.element"
    "Element"
    (with pkgs; [ element-desktop ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.etcher"
    "etcher"
    (with pkgs; [ gnome.gnome-disk-utility ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.freetube"
    "FreeTube"
    (with pkgs; [ freetube ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.gimp"
    "Gimp"
    (with pkgs; [ gimp ])
    { state.homeDirectories = [ ".config/GIMP/" ]; }
    {}
  ]

  [
    "nerv.opt.apps.gparted"
    "gparted"
    (with pkgs; [ gparted ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.hey"
    "HEY"
    (with pkgs; [ nerv.hey ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.inkscape"
    "Inkscape"
    (with pkgs; [ inkscape-with-extensions ])
    { state.homeDirectories = [ ".config/inkscape" ]; }
    {}
  ]

  [
    "nerv.opt.apps.logseq"
    "logseq"
    (with pkgs; [ logseq ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.lutris"
    "Lutris"
    (with pkgs; [ lutris openssl zenity ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.obs"
    "support for OBS"
    (with pkgs; [
      (wrapOBS {
        plugins = with obs-studio-plugins; [
          wlrobs
          obs-multi-rtmp
          obs-move-transition
          looking-glass-obs
        ];
      })
    ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.pcsx2"
    "PCSX2"
    (with pkgs; [ pcsx2 ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.pitivi"
    "Pitivi"
    (with pkgs; [ pitivi ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.pocketcasts"
    "Pocketcasts"
    (with pkgs; [ nerv.pocketcasts ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.prismlauncher"
    "Prism Launcher"
    (with pkgs; [ prismlauncher ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.protontricks"
    "Protontricks"
    (with pkgs; [ protontricks ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.qimgv"
    "Qt5 image viewer"
    (with pkgs; [ qimgv ])
    { state.homeDirectories = [ ".config/qimgv" ]; }
    {}
  ]

  [
    "nerv.opt.apps.rpcs3"
    "rpcs3"
    (with pkgs; [ rpcs3 ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.shotcut"
    "Shotcut"
    (with pkgs; [ shotcut ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.simplescreenrecorder"
    "SimpleScreenRecorder"
    (with pkgs; [ simplescreenrecorder ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.steamtinkerlaunch"
    "Steam Tinker Launch"
    (with pkgs; [ steamtinkerlaunch ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.telegram"
    "Telegram"
    (with pkgs; [ tdesktop ])
    { state.homeDirectories = [ ".local/share/TelegramDesktop" ]; }
    {}
  ]

  [
    "nerv.opt.apps.twitter"
    "Twitter"
    (with pkgs; [ nerv.twitter ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.vscode"
    "vscode"
    (with pkgs; [ vscode ])
    { state.homeDirectories = [ ".vscode" ".config/Code" ]; }
    {}
  ]

  [
    "nerv.opt.apps.vlc"
    "vlc"
    (with pkgs; [ vlc ])
    { state.homeDirectories = [ ".config/vlc" ]; }
    {}
  ]

  [
    "nerv.opt.apps.winetricks"
    "Winetricks"
    (with pkgs; [ winetricks ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.yt-music"
    "YouTube Music"
    (with pkgs; [ nerv.yt-music ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.yuzu"
    "Yuzu"
    (with pkgs; [ yuzu-mainline ])
    {}
    {}
  ]

  [
    "nerv.opt.apps.zathura"
    "Zathura Pdf viewer"
    (with pkgs; [
      (zathura.override {
        useMupdf = true;
      })
    ])
    { state.homeDirectories = [ ".config/zathura" ]; }
    {}
  ]
]
