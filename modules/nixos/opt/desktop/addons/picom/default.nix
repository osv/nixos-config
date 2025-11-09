{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.opt.desktop.addons.picom;
in {
  options.nerv.opt.desktop.addons.picom = with types; {
    enable = mkBoolOpt false
      "Whether to enable Picom X11 compositor.";
  };

  config = mkIf cfg.enable {
    # See https://config.phundrak.com/picom.html
    nerv.home.extraOptions.services.picom = {
      package = pkgs.picom;
      enable = true;
      backend = "glx";
      vSync = true;

      opacityRules = [
        # "100:class_g = 'Firefox'"
        # "100:class_g = 'Vivaldi-stable'"
        "100:class_g = 'VirtualBox Machine'"
        ''100:class_i = "VirtualBox Manager"''
        # Art/image programs where we need fidelity
        "100:class_g = 'Gimp'"
        "100:class_g = 'Inkscape'"
        "100:class_g = 'aseprite'"
        "100:class_g = 'krita'"
        "100:class_g = 'feh'"
        "100:class_g = 'mpv'"
        "100:class_g = 'Rofi'"
        "100:class_g = 'Peek'"
        "100:class_g = 'zoom'"
        "99:_NET_WM_STATE@:32a = '_NET_WM_STATE_FULLSCREEN'"
        ''100:role = "browser"''
        ''100:class_i = "mattermost"''
        ''100:name *= "Thunderbird"''
        ''100:name *= "Minecraft"''
        ''100:class_i = "DBeaver"''
        ''100:class_i = "emacs"''
        ''100:class_i = "nyxt"''
        ''100:class_i = "urxvt"''
        ''100:class_i = "element"''
        ''100:class_i = "DrRacket"''
        ''100:class_i = ".drracket-wrapped"''
        ''100:class_i = "code"''
      ];

      shadowExclude = [
        # Put shadows on notifications, the scratch popup and rofi only
        "! name~='(rofi|scratch|Dunst)$'"

        "name = 'Notification'"
        "_GTK_FRAME_EXTENTS@:c"
        "class_g = 'i3-frame'"
        "_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
        "_NET_WM_STATE@:32a *= '_NET_WM_STATE_STICKY'"
        "!I3_FLOATING_WINDOW@:c"
        "! name~=''"
        "n:e:Notification"
        "n:e:Plank"
        "n:e:Docky"
        "g:e:Synapse"
        "g:e:Kupfer"
        "g:e:Conky"
        "n:w:*Firefox*"
        "n:w:*Chrome*"
        "n:w:*Chromium*"
        "class_g ?= 'Notify-osd'"
        "class_g ?= 'Cairo-dock'"
        "class_g ?= 'Xfce4-notifyd'"
        "class_g ?= 'Xfce4-power-manager'"

        "class_g ?= 'xmonad-prompt'"
        "name = 'xmonad-prompt'"
      ];

      # fade = true;
      # fadeSteps = [0.1 0.1];

      # inactiveOpacity = 0.8;

      settings = {
        blur-background-exclude = [
          "window_type = 'dock'"
          "window_type = 'desktop'"
          "class_g = 'Rofi'"
          "_GTK_FRAME_EXTENTS@:c"
        ];

        # GLX backend: Avoid rebinding pixmap on window damage.
        # Probably could improve performance on rapid window content changes, but is known to break things on some drivers (LLVMpipe).
        # Recommended if it works.
        glx-no-rebind-pixmap = true;

        # Unredirect all windows if a full-screen opaque window is detected, to
        # maximize performance for full-screen windows. Known to cause
        # flickering when redirecting/unredirecting windows.
        unredir-if-possible = true;

        # GLX backend: Avoid using stencil buffer, useful if you don't have a
        # stencil buffer. Might cause incorrect opacity when rendering
        # transparent content (but never practically happened) and may not work
        # with blur-background. My tests show a 15% performance boost.
        # Recommended.
        glx-no-stencil = true;

        # Use X Sync fence to sync clients' draw calls, to make sure all draw
        # calls are finished before picom starts drawing. Needed on
        # nvidia-drivers with GLX backend for some users.
        xrender-sync-fence = true;

        xrender-sync = true;
      };
    };
  };
}
