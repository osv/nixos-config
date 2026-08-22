{ pkgs, ... }:

pkgs.mkShell {
  packages = with pkgs; [
    sops
    nixfmt
    shellcheck
  ];

  shellHook = ''
    echo "NixOS config development environment"
    echo "Available tools: sops, nixfmt, shellcheck"
  '';
}
