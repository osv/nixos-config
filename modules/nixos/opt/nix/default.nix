{ options, config, pkgs, lib, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.nix;

  my-nixos-diff = pkgs.writeShellScriptBin "my-nixos-diff" ''
#!/usr/bin/env bash
set -euo pipefail

# where NixOS system profiles live
PROFILE_DIR=/nix/var/nix/profiles

# resolve the symlink “system” → “system-N-link”
CURRENT_LINK=$(readlink "$PROFILE_DIR/system")
# extract just the number N
CURRENT_GEN=''${CURRENT_LINK#system-}
CURRENT_GEN=''${CURRENT_GEN%-link}

# previous generation number
PREV_GEN=$((CURRENT_GEN - 1))
PREV_LINK="system-''${PREV_GEN}-link"

# sanity check
if [ ! -e "$PROFILE_DIR/$PREV_LINK" ]; then
  echo "❌ Previous generation ($PREV_GEN) not found under $PROFILE_DIR"
  exit 1
fi

echo "🔍 Diffing NixOS generations $PREV_GEN → $CURRENT_GEN"
echo "  $PREV_GEN: $PROFILE_DIR/$PREV_LINK → $(readlink -f "$PROFILE_DIR/$PREV_LINK")"
echo "  $CURRENT_GEN: $PROFILE_DIR/system → $(readlink -f "$PROFILE_DIR/system")"
echo

# show what store paths were added (+) or removed (–)
nix store diff-closures \
  "$PROFILE_DIR/$PREV_LINK" \
  "$PROFILE_DIR/system"
'';

in
{
  options.nerv.opt.nix = with types; {
    enable = mkBoolOpt true "Whether or not to manage nix configuration.";
    package = mkOpt package pkgs.nixVersions.nix_2_28 "Which nix package to use. Be carefull with pkgs.nixUnstable";
  };

  config = mkIf cfg.enable {
    nerv.opt.persist.state.homeDirectories = [ ".local/share/nix" ];

    environment =
      {
        systemPackages = with pkgs; [
          nerv.nixos-revision
          deploy-rs
          nixfmt-classic
          nix-index
          nix-prefetch-git
          nix-output-monitor # sudo nixos-rebuild switch  -v --flake . |& nom
          nh # Yet another nix cli helper
          my-nixos-diff
        ];
        shellAliases = {
          nrs = "sudo nixos-rebuild switch  -v --flake . |& nom";
        };
      };

    nix =
      let users = [ "root" config.nerv.opt.user.name ];
      in
      {
        package = cfg.package;
        extraOptions = lib.concatStringsSep "\n" [
          ''
            experimental-features = nix-command flakes
            http-connections = 50
            warn-dirty = false
            log-lines = 50
            sandbox = relaxed
          ''
          (lib.optionalString (config.nerv.opt.tools.direnv.enable) ''
            keep-outputs = true
            keep-derivations = true
          '')
        ];

        settings = {
          experimental-features = "nix-command flakes";
          http-connections = 50;
          warn-dirty = false;
          log-lines = 50;
          sandbox = "relaxed";
          # auto-optimise-store = true;
          trusted-users = users;
          allowed-users = users;
        } // (lib.optionalAttrs config.nerv.opt.tools.direnv.enable {
          keep-outputs = true;
          keep-derivations = true;
        });

        # gc = {
        #   automatic = true;
        #   dates = "weekly";
        #   options = "--delete-older-than 30d";
        # };

        # flake-utils-plus
        generateRegistryFromInputs = true;
        generateNixPathFromInputs = true;
        linkInputs = true;
      };
  };
}
