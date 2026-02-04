{ pkgs, lib, ... }:

pkgs.writeShellScriptBin "my-generate-keybindings-index" (builtins.readFile ./my-generate-keybindings-index.sh)
