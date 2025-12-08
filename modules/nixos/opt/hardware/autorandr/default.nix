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
    };

    defaultProfile = mkOpt str "laptop" "Default profile when no external monitor detected";
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

        # Dual monitor profile (extended mode)
        dual = {
          fingerprint = {
            "${cfg.laptop.output}" = "*";
            "${cfg.external.output}" = "*";
          };
          config = {
            "${cfg.laptop.output}" = {
              enable = true;
              mode = cfg.laptop.mode;
              position = "2560x0"; # right of external
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
