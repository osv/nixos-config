{ config, lib, pkgs, ... }:

with lib;
with lib.nerv;

stdModules { inherit config lib pkgs; } [
  [
    "nerv.opt.system.time"
    "timezone information"
    []
    {}
    {
      time.timeZone = "Europe/Kiev";
    }
  ]

  [
    "nerv.opt.system.locale"
    "locale settings"
    []
    {}
    {
      i18n.defaultLocale = "en_US.UTF-8";
      console.keyMap = mkForce "us";
    }
  ]

  [
    "nerv.opt.system.boot"
    "booting"
    []
    {}
    {
    }
  ]
]
