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

  # Colored echo for system activation scripts (bold green)
  # Usage: ${sysEcho "Persist" "Creating home dirs..."}
  sysEcho = tag: msg: ''echo -e "\033[1;32m[${tag}]\033[0m ${msg}"'';

  # Colored echo for home activation scripts (bold blue)
  # Usage: ${homeEcho "Claude" "Setting up statusline..."}
  homeEcho = tag: msg: ''echo -e "\033[1;34m[${tag}]\033[0m ${msg}"'';

  # Create a test config script that symlinks repo files to home directory
  # This allows testing configuration changes without running nixos-rebuild switch.
  #
  # The script automatically detects if sourcePath is a file or directory.
  #
  # files list supports two formats:
  #   - String: "file.el" → source: $REPO/$sourcePath/file.el, target: $HOME/$targetPath/file.el
  #   - Attrset: { source = "lib/template.html"; target = "data/template.html"; }
  #     → source relative to repo root, target relative to targetPath
  #
  # Usage for directory source (most common):
  #   mkTestConfigScript pkgs {
  #     name = "my-test-emacs-config";
  #     appName = "Emacs Doom";
  #     sourcePath = "modules/nixos/opt/apps/emacs/config_doom";
  #     targetPath = ".config/doom";
  #     files = [
  #       "init.el" "config.el" "packages.el" "themes"
  #       { source = "lib/keybinding-template.html"; target = "keybinding-template.html"; }
  #     ];
  #     reloadCmd = "SPC h r r";
  #   }
  #
  # Usage for single file source (e.g., WezTerm):
  #   mkTestConfigScript pkgs {
  #     name = "my-test-wezterm-config";
  #     appName = "WezTerm";
  #     sourcePath = "modules/nixos/opt/desktop/addons/term/wezterm/config.lua";
  #     targetPath = ".config/wezterm";
  #     files = [ "wezterm.lua" ];
  #   }
  mkTestConfigScript = pkgs: { name, appName, sourcePath, targetPath, files, reloadCmd ? null }:
    let
      repoPath = "/home/osv/work/my/nixos-config";

      # Split files into simple (strings) and custom (attrsets)
      simpleFiles = filter isString files;
      customFiles = filter isAttrs files;

      reloadHint = optionalString (reloadCmd != null) ''

        echo ""
        echo "Reload ${appName} with: ${reloadCmd}"'';

      # File listing for output
      filesList = concatMapStringsSep "" (f: ''
        echo "  $HOME/${targetPath}/${f}"
      '') simpleFiles
      + concatMapStringsSep "" (f: ''
        echo "  $HOME/${targetPath}/${f.target}"
      '') customFiles;

      simpleSpaced = concatStringsSep " " simpleFiles;

      # rm commands for simple files
      rmSimple = concatMapStringsSep "" (f: ''
        rm -rf "$TARGET/${f}"
      '') simpleFiles;

      # rm commands for custom files
      rmCustom = concatMapStringsSep "" (f: ''
        rm -rf "$TARGET/${f.target}"
      '') customFiles;

      # ln commands for custom files (source relative to repo root)
      lnCustom = concatMapStringsSep "" (f: ''
        mkdir -p "$(dirname "$TARGET/${f.target}")"
        ln -sv "$REPO/${f.source}" "$TARGET/${f.target}"
      '') customFiles;
    in pkgs.writeShellScriptBin name ''
      # IMPORTANT: Keep this list in sync with actual config files!
      REPO="${repoPath}"
      SOURCE="$REPO/${sourcePath}"
      TARGET="$HOME/${targetPath}"

      echo "Setting up ${appName} config symlinks..."

      # Check if source exists
      if [ ! -e "$SOURCE" ]; then
        echo "Error: Source not found: $SOURCE"
        exit 1
      fi

      # Create target directory if needed
      mkdir -p "$TARGET"

      # Remove only the files we're going to replace (preserve other files like themes)
      # Fix read-only permissions (rsync from Nix store creates read-only dirs)
      echo "Removing old symlinks..."
      chmod -R u+w "$TARGET" 2>/dev/null || true
      ${rmSimple}${rmCustom}

      # Create symlinks - auto-detect if source is file or directory
      echo "Creating symlinks..."
      if [ -f "$SOURCE" ]; then
        # Source is a single file - link it to each target filename
        for file in ${simpleSpaced}; do
          ln -sv "$SOURCE" "$TARGET/$file"
        done
      else
        # Source is a directory - link each file from it
        for file in ${simpleSpaced}; do
          ln -sv "$SOURCE/$file" "$TARGET/$file"
        done
      fi

      # Custom files (source relative to repo root)
      ${lnCustom}

      echo "Done! Symlinks created:"
      ${filesList}${reloadHint}
    '';

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
