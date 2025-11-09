{ pkgs, ... }:

pkgs.mkShell {
  packages = with pkgs; [
    python3
    uv
    # Common Python development tools
    ruff
    pyright
  ];

  shellHook = ''
    echo "Python ${pkgs.python3.version} development environment"
    echo "Available tools: python3, uv, ruff, pyright"
  '';
}