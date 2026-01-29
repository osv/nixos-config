{ pkgs, lib, ... }:

pkgs.writeShellScriptBin "nix-update-index" ''
  set -euo pipefail

  filename="index-${pkgs.stdenv.hostPlatform.system}"
  release="https://github.com/Mic92/nix-index-database/releases/latest/download/''${filename}"

  mkdir -p ~/.cache/nix-index

  pushd ~/.cache/nix-index > /dev/null

  echo "Downloading https://github.com/Mic92/nix-index-database/releases/latest/download/$filename"
  ${pkgs.wget}/bin/wget -q -N https://github.com/Mic92/nix-index-database/releases/latest/download/$filename

  ln -f ''${filename} files

  popd > /dev/null
''
