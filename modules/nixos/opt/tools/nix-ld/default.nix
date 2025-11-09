{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let cfg = config.nerv.opt.tools.nix-ld;
in
{
  options.nerv.opt.tools.nix-ld = with types; {
    enable = mkBoolOpt false "Whether or not to enable nix-ld.";
  };

  config = mkIf cfg.enable {
    programs.nix-ld.enable = true;
    # Lets play X4_nosteam without paing. TODO: why no sound :(
    # See https://github.com/Mic92/dotfiles/blob/main/nixos/modules/nix-ld.nix
    programs.nix-ld.libraries = with pkgs; [
      alsa-lib
      at-spi2-atk
      at-spi2-core
      atk
      bzip2
      cairo
      cups
      curl
      dbus
      expat
      fontconfig
      freetype
      fuse3
      gdk-pixbuf
      glib
      gtk3
      icu
      libGL
      libappindicator-gtk3
      libdrm
      libglvnd
      libnotify
      libpulseaudio
      libunwind
      libusb1
      libuuid
      libxkbcommon
      libxml2
      mesa
      nspr
      nss
      openal
      openssl
      pango
      pipewire
      stdenv.cc.cc
      systemd
      vulkan-loader
      xorg.libX11
      xorg.libXScrnSaver
      xorg.libXcomposite
      xorg.libXcursor
      xorg.libXdamage
      xorg.libXext
      xorg.libXfixes
      xorg.libXi
      xorg.libXrandr
      xorg.libXrender
      xorg.libXtst
      xorg.libxcb
      xorg.libxkbfile
      xorg.libxshmfence
      zlib
    ];
  };
}
