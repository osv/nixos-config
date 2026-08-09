{ config, lib, pkgs, ... }:
let
  # Based on https://gitlab.com/K900/nix.git (push)
  # Mainly useful for KDE config files
  cfg = config.nerv.opt.desktop.addons.plasma-hack;

  setValue = v:
    let
      setValueArgs = ty: vs: "--type ${ty} ${lib.escapeShellArg vs}";
    in
    if builtins.isBool v
    then
      setValueArgs "bool"
        (
          if v
          then "true"
          else "false"
        )
    else setValueArgs "str" (builtins.toString v);

  pathToArgs = path:
    let
      groupArg = item: "--group ${lib.escapeShellArg item}";
      groupArgs = builtins.map groupArg path;
    in
    groupArgs;

  entryToArgs =
    { path
    , value
    ,
    }:
    let
      file = builtins.head path;
      subpath = builtins.tail path;
      groups = lib.lists.init subpath;
      name = lib.lists.last subpath;

      fileArg = "--file ${lib.escapeShellArg file}";
      pathArgs = pathToArgs groups;
      keyArg = "--key ${lib.escapeShellArg name}";
      valueArg = setValue value;
      allArgs = pathArgs ++ [ fileArg keyArg valueArg ];
    in
    lib.strings.concatStringsSep " " allArgs;

  flattenAttrs = attrs: pathSoFar:
    lib.lists.flatten (lib.attrsets.mapAttrsToList
      (
        name: value:
          if builtins.isAttrs value
          then flattenAttrs value (pathSoFar ++ [ name ])
          else {
            path = pathSoFar ++ [ name ];
            inherit value;
          }
      )
      attrs);

  configToArgs = attrs: builtins.map entryToArgs (flattenAttrs attrs [ ]);

  configToScript = attrs:
    let
      args = configToArgs attrs;
      # NixOS 26.05: Plasma 5 removed, use kdePackages.kconfig
      argToCommand = arg: "${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 ${arg}";
      commands = builtins.map argToCommand args;
    in
    lib.strings.concatStringsSep "\n" commands;

  writeConfig = attrs: pkgs.writeScript "kconfig-setup" (configToScript attrs);
in
{
  options.nerv.opt.desktop.addons.plasma-hack = {
    kconfig = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "Config files needed to update by kwriteconfig5 tool";
    };

    wallpaper = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
    };
  };

  config = lib.mkMerge [
    (lib.mkIf (cfg.kconfig != { }) {
      # Apply KDE config via systemd user service (runs at every boot/login)
      nerv.home.extraOptions.systemd.user.services.kconfig-setup = {
        Unit = {
          Description = "Apply KDE config from Nix configuration";
          After = [ "graphical-session-pre.target" ];
          Before = [ "plasma-plasmashell.service" "graphical-session.target" ];
          PartOf = [ "graphical-session.target" ];
        };
        Service = {
          Type = "oneshot";
          RemainAfterExit = true;
          ExecStart = writeConfig cfg.kconfig;
        };
        Install.WantedBy = [ "graphical-session.target" ];
      };
    })
    # TODO: Test wallpaper service
    (lib.mkIf (cfg.wallpaper != null) {
      systemd.user.services.hm-setup-plasma-wallpaper = {
        Unit = {
          Description = "Set up Plasma wallpaper from home-manager config";
          After = [ "plasma-plasmashell.service" ];
          Requires = [ "plasma-plasmashell.service" ];
        };

        Service = {
          Type = "oneshot";
          # NixOS 26.05: Plasma 5 removed, use kdePackages.plasma-workspace
          ExecStart = "${pkgs.kdePackages.plasma-workspace}/bin/plasma-apply-wallpaperimage ${lib.escapeShellArg cfg.wallpaper}";
        };

        Install.WantedBy = [ "plasma-workspace.target" ];
      };
    })
  ];
}
