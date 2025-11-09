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
      argToCommand = arg: "${pkgs.plasma5Packages.kconfig}/bin/kwriteconfig5 ${arg}";
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
      nerv.home.activation.kconfig-setup = "$DRY_RUN_CMD ${writeConfig cfg.kconfig}";
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
          ExecStart = "${pkgs.plasma5Packages.plasma-workspace}/bin/plasma-apply-wallpaperimage ${lib.escapeShellArg cfg.wallpaper}";
        };

        Install.WantedBy = [ "plasma-workspace.target" ];
      };
    })
  ];
}
