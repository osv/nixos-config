{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.nerv;

let
  cfg = config.nerv.opt.hardware.autorandr;

  # Fake EDID that will never match any real monitor
  # Used for profiles that should only be activated manually
  fakeEdid = "00ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff00";

  # Helper for dual-monitor profile fingerprints
  # active = true → real fingerprint (auto-selected)
  # active = false → fake EDID fingerprint (manual only, never auto-matches)
  mkDualFingerprint = { active, includeLaptop ? true }:
    if active then {
      "${cfg.laptop.output}" = "*";
      "${cfg.external.output}" = "*";
    } else
      # Use fake EDID for outputs that are enabled in config
      # This satisfies autorandr's requirement while preventing auto-match
      (if includeLaptop then {
        "${cfg.laptop.output}" = fakeEdid;
        "${cfg.external.output}" = fakeEdid;
      } else {
        "${cfg.external.output}" = fakeEdid;
      });
in
{
  options.nerv.opt.hardware.autorandr = with types; {
    enable = mkBoolOpt false "Whether or not to enable autorandr (automatic display configuration)";

    laptop = {
      output = mkOpt str "eDP-1" "Laptop display output name";
      mode = mkOpt str "1920x1080" "Laptop display resolution";
    };

    external = {
      output = mkOpt str "HDMI-1" "External display output name";
      mode = mkOpt str "2560x1440" "External display resolution";
      width = mkOpt str (builtins.head (lib.splitString "x" cfg.external.mode))
        "External display width in pixels (used for positioning laptop display in dual mode)";
    };

    defaultProfile = mkOpt str "laptop" "Default profile when no external monitor detected";

    dualMode = mkOpt (enum [ "extend" "external-only" ]) "external-only"
      "Behavior when both monitors are connected: 'extend' - use both monitors, 'external-only' - disable laptop display and use only external";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.autorandr ];

    # Override systemd service to prevent race condition with xmonad
    systemd.services.autorandr = {
      # Add delay to give X and xmonad time to initialize
      serviceConfig = {
        ExecStartPre = "${pkgs.coreutils}/bin/sleep 3";
      };
      # Don't start until X server creates socket
      unitConfig = {
        ConditionPathExists = "/tmp/.X11-unix/X0";
      };
    };

    services.autorandr = {
      enable = true;
      defaultTarget = cfg.defaultProfile;

      profiles = {
        laptop = {
          fingerprint = {
            "${cfg.laptop.output}" = "*";
          };
          config = {
            "${cfg.laptop.output}" = {
              enable = true;
              primary = true;
              mode = cfg.laptop.mode;
              position = "0x0";
            };
          };
        };

        external = {
          fingerprint = {
            "${cfg.external.output}" = "*";
          };
          config = {
            "${cfg.external.output}" = {
              enable = true;
              primary = true;
              mode = cfg.external.mode;
              position = "0x0";
            };
          };
        };

        # External only mode when both monitors connected
        external-only = {
          fingerprint = mkDualFingerprint {
            active = cfg.dualMode == "external-only";
            includeLaptop = false;  # laptop is disabled in this profile
          };
          config = {
            "${cfg.laptop.output}" = {
              enable = false;
            };
            "${cfg.external.output}" = {
              enable = true;
              primary = true;
              mode = cfg.external.mode;
              position = "0x0";
            };
          };
        };

        # Dual monitor profile (extended mode)
        dual = {
          fingerprint = mkDualFingerprint {
            active = cfg.dualMode == "extend";
            # includeLaptop = true by default (both monitors enabled)
          };
          config = {
            "${cfg.laptop.output}" = {
              enable = true;
              mode = cfg.laptop.mode;
              position = "${cfg.external.width}x0";
            };
            "${cfg.external.output}" = {
              enable = true;
              primary = true;
              mode = cfg.external.mode;
              position = "0x0";
            };
          };
        };
      };

      hooks = {
        postswitch = {
          # # Restart XMonad after profile switch
          # # Give X server time to apply changes
          # "restart-xmonad" = ''
          #   sleep 1
          #   if command -v xmonad &> /dev/null; then
          #     # If xmonad crashed, it will restart via display manager
          #     # If alive — do restart
          #     if pgrep -x xmonad &> /dev/null; then
          #       xmonad --restart || true
          #     fi
          #   fi
          # '';
          "notify" = ''
            if command -v notify-send &> /dev/null; then
              notify-send "Display" "Switched to profile: $AUTORANDR_CURRENT_PROFILE"
            fi
          '';
          "set-wallpaper" = ''
            if command -v my-set-wallpaper &> /dev/null; then
              my-set-wallpaper || true
            fi
          '';
        };
      };
    };
  };
}
