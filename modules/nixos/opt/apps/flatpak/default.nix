{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;

let cfg = config.nerv.opt.apps.flatpak;
in {
  options.nerv.opt.apps.flatpak = with types; {
    enable = mkBoolOpt false "Whether or not to enable flatpak (containerised app runtime with flathub remote).";
    # flatpak app ids declared by other modules (Signal, Steam, ...). A non-empty
    # list auto-enables flatpak (unless explicitly disabled) and is forwarded to
    # the nix-flatpak home-manager module for installation.
    packages = mkOpt (listOf str) [ ] "flatpak app ids to install (e.g. \"org.signal.Signal\"). Auto-enables flatpak when non-empty.";
  };

  config = mkMerge [
    # Auto-enable flatpak when at least one flatpak app is declared. mkDefault lets
    # a suite/host explicitly opt out (nerv.opt.apps.flatpak = off / enable = false).
    (mkIf (cfg.packages != [ ]) {
      nerv.opt.apps.flatpak.enable = mkDefault true;
    })

    (mkIf cfg.enable {
      # System flatpak daemon. Only `enable` / `package` / `extraPortals` exist at
      # the NixOS level (nixpkgs built-ins). Declarative management (remotes /
      # update / packages / overrides) lives in the nix-flatpak HOME-MANAGER module.
      services.flatpak.enable = true;

      # Flatpak needs XDG desktop portals; bring the portal addon along (mkDefault
      # so a host/suite can still control it explicitly). nixpkgs hard-asserts
      # xdg.portal.enable when services.flatpak.enable is true.
      nerv.opt.desktop.addons.xdg-portal.enable = mkDefault true;

      nerv.home.extraOptions.services.flatpak = {
        remotes = mkOptionDefault [
          { name = "flathub"; location = "https://dl.flathub.org/repo/flathub.flatpakrepo"; }
        ];
        # Auto-update all flatpaks once a week on Sunday daytime (user systemd timer).
        update.auto = {
          enable = true;
          onCalendar = "Sun 14:00";
        };
        # Apps declared via nerv.opt.apps.flatpak.packages (Signal, Steam, ...).
        packages = cfg.packages;
        # Expose system icons / fonts / themes to every flatpak app. Works on NixOS
        # because /nix/store is visible inside the sandbox by default, so symlinks
        # under /run/current-system/sw/share (e.g. Papirus → /nix/store/...) resolve.
        # Apps search themes via XDG_DATA_DIRS / XCURSOR_PATH, so we expose the path
        # AND add it to those env vars (flatpak's default bridge points at
        # /run/host/share = host /usr/share, which is empty on NixOS).
        overrides.settings.global = {
          Context.filesystems = [ "/run/current-system/sw/share:ro" ];
          Environment = {
            XDG_DATA_DIRS = "/run/current-system/sw/share:/app/share:/usr/share:/usr/share/runtime/share";
            XCURSOR_PATH = "/run/current-system/sw/share/icons";
          };
        };
      };

      # Package / runtime storage is re-downloadable → derivative (never auto-cleaned).
      nerv.opt.persist.derivative = {
        directories = [ "/var/lib/flatpak" ];
        homeDirectories = [ ".local/share/flatpak" ];
      };

      # Real app user data (chat history, configs) → state. Covers all flatpak apps.
      nerv.opt.persist.state.homeDirectories = [ ".var/app" ];
    })

    # Flatpak needs XDG desktop portals. The module brings nerv.opt.desktop.addons.xdg-portal
    # along (mkDefault above); warn if it ended up disabled (e.g. a host set it off).
    # Note: when the portal is fully off, nixpkgs itself hard-asserts xdg.portal.enable.
    {
      warnings = optional (cfg.enable && !config.nerv.opt.desktop.addons.xdg-portal.enable)
        "nerv.opt.apps.flatpak is enabled but nerv.opt.desktop.addons.xdg-portal is not. Flatpak apps require XDG desktop portals (FileChooser, Settings, OpenURI) — enable nerv.opt.desktop.addons.xdg-portal.";
    }
  ];
}
