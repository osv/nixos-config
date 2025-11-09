{ config, lib, pkgs, ... }:

with lib;
with lib.nerv;

stdModules { inherit config lib pkgs; } [
  [
    "nerv.opt.services.printing"
    "printing support"
    []
    {}
    {
      services.printing.enable = true;
    }
  ]
]
