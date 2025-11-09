{ config, pkgs, lib, inputs, ... }:
with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.persist;
  userName = config.nerv.opt.user.name;

  homeDir = "/home/${userName}";

  takeAll = what: concatMap (x: x.${what});

  persists = with cfg; [ state derivative cache ];
  persistsKinds = [ "state" "derivative" "cache" ];

  absoluteHomeFiles = map (x: "${homeDir}/${x}");

  allHomeFiles = takeAll "homeFiles" persists;
  allFiles = takeAll "files" persists;
  allDirectories = takeAll "directories" persists;
  allHomeDirectories = takeAll "homeDirectories" persists;

  inherit (builtins) concatMap;
  inherit (lib) mkIf;

  inherit (lib.types) listOf path str anything;

  commonOptions = {
    directories = mkOpt' (listOf anything) [ ];
    files = mkOpt' (listOf str) [ ];
    homeDirectories = mkOpt' (listOf anything) [ ];
    homeFiles = mkOpt' (listOf str) [ ];
  };

  homeDirectoryToDirectory = dir:
    if (isString dir) then {
      directory = "${homeDir}/${dir}";
      user = userName;
      group = "users";
    } else
      ({
        user = userName;
        group = "users";
      } // dir // {
        directory = "${homeDir}/${dir.directory}";
      });

  my-show-not-persisted-files =
    pkgs.writeShellScriptBin "my-show-not-persisted-files" ''
      ${lib.getExe pkgs.fd} \
         --hidden \
         --absolute-path \
         --one-file-system \
         --type file \
         --color always --base-directory / \
         . | sort --unique | grep -P -v '^(?:\x1B\[[0-9;]*[A-Za-z])*\/tmp\/'
    '';
in {
  options.nerv.opt.persist = {

    enable = mkBoolOpt false "Whether or not to enable persistent state.";

    persistHome = mkBoolOpt true
      "Whether or not to enable persistent state of homeDirectories and homeFiles.";

    persistRootDir = mkOption {
      type = path;
      default = "/persist";
      description = ''
        The path to persistent storage where the real file should be stored.
        Will be created "persists" sub directories, e.g for /persists:
        /persists/state
        /persists/derivative
        /persists/cache
      '';
    };

    # Stuff that matters
    state = commonOptions;

    # Stuff that can be computed from declarative+state, but is never invalidated (so shouldn't be cleaned up)
    derivative = commonOptions;

    # Stuff that's just there to speed up the system
    # It's cleaned up regularly, to solve the cache invalidation problem once and for all
    cache = {
      clean = {
        enable = mkBoolOpt false "Cleaning the cache files and directories.";

        dates = mkOption {
          type = str;
          default = "weekly";
          description =
            "A systemd.time calendar description of when to clean the cache files";
        };
      };
    } // commonOptions;
  };

  imports = [
    inputs.impermanence.nixosModules.impermanence
    # # Eugh
    # (let
    #   module = (import "${inputs.impermanence}/home-manager.nix" {
    #     inherit pkgs lib;
    #     config = lib.recursiveUpdate config.home-manager.users.${userName} {
    #       home.persistence = (builtins.listToAttrs (map (p: {
    #         name = "${cfg.persistRootDir}/${p}${homeDir}";
    #         value = {
    #           directories = [ ]; # cfg.${p}.homeDirectories;
    #           files = cfg.${p}.homeFiles;
    #           allowOther = false;
    #           removePrefixDirectory = false;
    #         };
    #       }) persistsKinds));
    #     };
    #   });
    # in {
    #   config.home-manager.users.${userName} =
    #     lib.mkIf (cfg.enable && cfg.persistHome) module.config;
    # })
  ];

  config = mkIf cfg.enable {
    environment.systemPackages = [ my-show-not-persisted-files ];

    environment.persistence = (builtins.listToAttrs (map (p: {
      name = "${cfg.persistRootDir}/${p}";
      value = {
        directories = cfg.${p}.directories ++ (optionals cfg.persistHome
          (map homeDirectoryToDirectory cfg.${p}.homeDirectories));
        files = cfg.${p}.files;
        hideMounts = true;
        users.${userName} = {
          files=cfg.${p}.homeFiles;
        };
      };
    }) persistsKinds));

    # boot.initrd.postMountCommands = assert config.fileSystems
    #   ? ${cfg.persistRootDir}
    #   && config.fileSystems.${cfg.persistRootDir}.neededForBoot; ''
    #     mkdir -p /mnt-root/nix
    #     mount --bind /mnt-root${cfg.persistRootDir}/nix /mnt-root/nix
    #     chmod 755 /mnt-root
    #   '';

    # Euuuugh
    systemd.services.persist-cache-cleanup = lib.mkIf cfg.cache.clean.enable {
      description = "Cleaning up cache files and directories";
      script = ''
        ${builtins.concatStringsSep "\n" (map (x: "rm ${lib.escapeShellArg x}")
          (cfg.cache.files ++ absoluteHomeFiles cfg.cache.homeFiles))}

        ${builtins.concatStringsSep "\n"
        (map (x: "rm -rf ${lib.escapeShellArg x}") cfg.cache.directories
          ++ absoluteHomeFiles cfg.cache.homeDirectories)}
      '';
      startAt = cfg.cache.clean.dates;
    };

    system.activationScripts = {
      homedir.text = ''
        echo "[Persist] Creating home dirs ${cfg.persistRootDir}/{state,derivative,cache}/home"
        mkdir -p ${cfg.persistRootDir}/state/home
        mkdir -p ${cfg.persistRootDir}/derivative/home
        mkdir -p ${cfg.persistRootDir}/cache/home
        chown ${userName}:users ${cfg.persistRootDir}/state/home
        chown ${userName}:users ${cfg.persistRootDir}/derivative/home
        chown ${userName}:users ${cfg.persistRootDir}/cache/home
      '';
      #  + builtins.concatStringsSep "\n" (map
      #   (dir: ''
      #     mkdir -p ${cfg.persistRootDir}${dir}
      #     chown ${userName}:users ${cfg.persistRootDir}${dir}
      #   '')
      #   (builtins.filter (dir: (isString dir) && (hasPrefix "/home/${userName}" dir))
      #     (allDirectories ++ (map homeDirectoryToDirectory allHomeDirectories))))
      # + ''
      #   echo "[Persist] Done"
      # '';
    };
  };
}
