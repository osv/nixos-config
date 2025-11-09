{ config, lib, pkgs, ... }:

with lib;
with lib.nerv;

stdModules { inherit config lib pkgs; } [
  [
    "nerv.opt.hardware.fingerprint"
    "fingerprint support"
    []
    {}
    {
      services.fprintd.enable = true;
    }
  ]
]
