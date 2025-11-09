{ config, lib, pkgs, ... }:

with lib;
with lib.nerv;

stdModules { inherit config lib pkgs; } [
  [
    "nerv.opt.security.keyring"
    "gnome keyring"
    (with pkgs; [ gnome-keyring libgnome-keyring ])
    {}
    {}
  ]
]
