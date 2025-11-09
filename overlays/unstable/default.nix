{ channels, ... }:

final: prev: {
  inherit (channels.unstable) 
    emacs-lsp-booster 
    picom
    kubecolor
    gamescope
    firefox-wayland
    wrapOBS obs-studio obs-studio-plugins
    freetube
    flyctl
    nixUnstable
    lutris
    prismlauncher
    chromium
    deploy-rs
    viber
    zoom-us
    nerd-fonts
    nordic;
    
  tmuxPlugins = prev.tmuxPlugins // {
    inherit (channels.unstable.tmuxPlugins) vim-tmux-navigator;
  };
}
