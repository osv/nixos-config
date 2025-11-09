{ claude-desktop, ... }:

final: prev: {
  claude-desktop = claude-desktop.packages.${final.system}.claude-desktop;
}
