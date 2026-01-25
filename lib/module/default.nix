{ lib, ... }:

with lib; rec {
  mkOpt = type: default: description:
    mkOption { inherit type default description; };

  mkOpt' = type: default: mkOpt type default null;

  mkBoolOpt = mkOpt types.bool;

  mkBoolOpt' = mkOpt' types.bool;

  enabled = { enable = true; };
  disabled = { enable = false; };

  on = { enable = true; };
  off = { enable = false; };

  yes = true;
  no = false;

  fuckOff = { enable = lib.mkForce false; };
  fuckYes = lib.mkForce true;

  # Colored echo for system activation scripts (magenta)
  # Usage: ${sysEcho "Persist" "Creating home dirs..."}
  sysEcho = tag: msg: ''echo -e "\033[0;35m[${tag}]\033[0m ${msg}"'';

  # Colored echo for home activation scripts (blue)
  # Usage: ${homeEcho "Claude" "Setting up statusline..."}
  homeEcho = tag: msg: ''echo -e "\033[0;34m[${tag}]\033[0m ${msg}"'';

  # Simplify creation of multiple standard modules in a single file
  # Usage: stdModules args [ [path description packages persistConfig extraConfig] ... ]
  # Example:
  #   stdModules args [
  #     [
  #       "nerv.opt.cli-apps.wine"
  #       "Wine"
  #       (with pkgs; [winePackages.unstable winetricks])
  #       { derivative.homeDirectories = [ ".local/share/wineprefixes/default" ]; }
  #       {}
  #     ]
  #   ]
  stdModules = args@{ config, lib, pkgs, ... }: modulesList:
    with lib;
    with lib.nerv;
    let
      # Convert "nerv.opt.cli-apps.wine" to ["nerv" "opt" "cli-apps" "wine"]
      pathToList = path: splitString "." path;

      # Create nested attrset from path list
      # ["nerv" "opt" "wine"] { enable = ...; } => { nerv = { opt = { wine = { enable = ...; }; }; }; }
      mkNestedAttr = path: value:
        let
          parts = pathToList path;
          go = parts: value:
            if parts == [] then value
            else { ${head parts} = go (tail parts) value; };
        in go parts value;

      # Process a single module definition
      processModule = moduleDef:
        let
          path = elemAt moduleDef 0;
          description = elemAt moduleDef 1;
          packages = elemAt moduleDef 2;
          persistConfig = if length moduleDef > 3 then elemAt moduleDef 3 else {};
          extraConfig = if length moduleDef > 4 then elemAt moduleDef 4 else {};

          pathList = pathToList path;
          cfg = getAttrFromPath pathList config;

          # Generate the enable option
          optionDef = mkNestedAttr path {
            enable = mkBoolOpt false "Whether or not to enable ${description}.";
          };

          # Build persistence config with proper structure
          persistCfg = persistConfig;

          # Generate the config
          configDef = mkIf cfg.enable (mkMerge [
            (optionalAttrs (packages != []) {
              environment.systemPackages = packages;
            })
            (optionalAttrs (persistConfig != {}) {
              nerv.opt.persist = persistCfg;
            })
            extraConfig
          ]);
        in
          { options = optionDef; config = configDef; };

      # Process all modules
      processed = map processModule modulesList;

      # Merge all options and configs
      mergedOptions = foldl' recursiveUpdate {} (map (m: m.options) processed);
      mergedConfigs = map (m: m.config) processed;
    in
      {
        options = mergedOptions;
        config = mkMerge mergedConfigs;
      };
}
