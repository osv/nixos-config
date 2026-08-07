{ channels, ... }:

final: prev: {
  # Clickhouse-cli from unstable (updated to Python 3.13)
  # Previously needed sqlparse override for version compatibility, but
  # newer unstable versions have updated clickhouse-cli that works
  # with current sqlparse. Override removed as it causes build failures.
  clickhouse-cli = channels.unstable.python313Packages.clickhouse-cli;
}
