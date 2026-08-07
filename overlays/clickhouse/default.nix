{ channels, ... }:

final: prev: {
  # clickhouse-cli - TEMPORARILY BROKEN
  #
  # Problem: clickhouse-cli (0.3.9) is incompatible with sqlparse 0.5.5 in unstable
  # - Unstable uses Python 3.13 with sqlparse 0.5.5
  # - sqlparse 0.5.5 changed SQL_REGEX structure: (regex, token) → (regex, token, flags)
  # - clickhouse-cli expects old format → ValueError: too many values to unpack (expected 2)
  # - See: https://github.com/hatarist/clickhouse-cli/issues/50
  #
  # Why this happened:
  # - Python 3.11 was removed from unstable (only 3.13/3.14 remain)
  # - sqlparse 0.4.3 (compatible) doesn't build with Python 3.13
  # - All unstable Python versions use sqlparse 0.5.5 (incompatible)
  #
  # Workarounds to install clickhouse-cli:
  #
  # 1. Use nixpkgs (25.11) instead of unstable:
  #    channels.nixpkgs.clickhouse-cli
  #
  # 2. Create separate stable channel in flake.nix:
  #    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-25.11";
  #    channels.nixpkgs-stable.clickhouse-cli
  #
  # 3. Install manually in home-manager:
  #    home.packages = [ pkgs.clickhouse-cli ];
  #
  # 4. Use alternative ClickHouse interactive clients:
  #    - clickhouse-client (official CLI from ClickHouse)
  #    - clickhouse-driver (Python, can be used interactively)
  #    - clickhouse-connect (Python, good for scripts)
  #
  # To re-enable: uncomment below and choose one of the workarounds above

  # clickhouse-cli = channels.nixpkgs.clickhouse-cli;
}
