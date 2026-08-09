{ claude-desktop, ... }:

final: prev: {
  claude-desktop = claude-desktop.packages.${final.stdenv.hostPlatform.system}.claude-desktop;
}
