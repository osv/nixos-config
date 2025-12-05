{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.desktop.addons.file;
  useGuiFileManager = cfg.gnomeFile.enable || cfg.kdeFile.enable;
in
{
  options.nerv.opt.desktop.addons.file = with types; {
    enable = mkBoolOpt false "Whether to enable file manager and utils";

    letsMountWindows = mkBoolOpt false "Whether to enable mounting windows partitions. Usefull if you are in dualboot.";
    letsMountISO = mkBoolOpt false "Whether to enable mounting windows partitions. Usefull if you are in dualboot.";
    withSambaBrowse = mkBoolOpt false "Whether to enable console utils like nnn";

    consoleFile.enable = mkBoolOpt false "Whether to enable the console file managers (nnn, ncdu, etc)";
    gnomeFile.enable = mkBoolOpt false "Whether to enable the gnome file manager, mainly Nautilus.";
    kdeFile.enable = mkBoolOpt false "Whether to enable the KDE file manager, mainly Dolphin.";
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.letsMountWindows && useGuiFileManager) {
      environment.systemPackages = with pkgs; [ ntfs3g ];

      # Mount filesystem
      security.polkit = on // {
        extraConfig = ''
          polkit.addRule(function(action, subject) {
             if (action.id == 'org.freedesktop.udisks2.filesystem-mount-system' && subject.isInGroup('wheel')) {
                 return polkit.Result.YES;
             }});
        '';
      };
    })

    (mkIf (cfg.enable && cfg.letsMountISO && useGuiFileManager) {
      environment.systemPackages = with pkgs; [ fuseiso ];
    })

    (mkIf (cfg.enable && cfg.withSambaBrowse && useGuiFileManager) {
      # Enable support for browsing samba shares.
      services.gvfs.enable = true;
      networking.firewall.extraCommands =
        "iptables -t raw -A OUTPUT -p udp -m udp --dport 137 -j CT --helper netbios-ns";
    })

    (mkIf (cfg.enable && cfg.consoleFile.enable) {
      environment.systemPackages = with pkgs; [
        nnn
        yazi                    # like emacs dirvish
        ncdu                    # interactive du replacement
        du-dust                 # `dust` - tree directories
        duf                     # du-like but with table
      ];
    })

    (mkIf (cfg.enable && cfg.gnomeFile.enable) {
      environment.systemPackages = with pkgs; [ nautilus file-roller nautilus-python ];
      nerv.home.extraOptions = {
        dconf.settings = {
          "org/gnome/nautilus/icon-view" = {
            captions = [ "size" "date_modified" "none" ];
          };

          "org/gnome/nautilus/list-view" = {
            default-column-order = [
              "name"
              "size"
              "type"
              "owner"
              "group"
              "permissions"
              "where"
              "date_modified"
              "date_modified_with_time"
              "date_accessed"
              "recency"
              "starred"
              "detailed_type"
            ];
            default-visible-columns = [ "name" "size" "date_modified" "starred" ];
          };

          "org/gnome/nautilus/preferences" = {
            default-folder-viewer = "list-view";
            executable-text-activation = "display";
            search-filter-time-type = "last_modified";
            search-view = "list-view";
            show-image-thumbnails = "always";
            thumbnail-limit = 10;
          };
        };
      };
    })

    (mkIf (cfg.enable && cfg.kdeFile.enable) {
      nerv.opt.desktop.addons.plasma-hack.kconfig = {
        kdeglobals = {
          General.TerminalApplication=config.nerv.opt.desktop.addons.term.defaultTerminal;
        };
      };

      nerv.opt.persist.state = {
        homeFiles = [
          ".config/dolphinrc"
          # ".local/share/recently-used.xbel" # recently used application, stored by KDE, I don't need it
          ".local/share/user-places.xbel"
          ".local/share/user-places.xbel.bak"
        ];
        homeDirectories = [
          ".local/share/dolphin"
          # ".local/share/RecentDocuments"
        ];
      };

      environment.systemPackages = with pkgs.libsForQt5;
        with pkgs; [
          qdirstat
          k4dirstat

          kdePackages.dolphin
          kdePackages.dolphin-plugins # TODO: integrate Dolphin with th Git, Dropbox
          kdePackages.kdegraphics-thumbnailers # PostScript and Raw
          kdePackages.ffmpegthumbs
          kdePackages.kio-extras
          kdePackages.kio-fuse
          # kio-s3
        ] ++ optional cfg.withSambaBrowse pkgs.libsForQt5.kdenetwork-filesharing;
    })
  ];
}
