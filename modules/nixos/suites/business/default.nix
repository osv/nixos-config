{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.suites.business;
in
{
  options.nerv.suites.business = with types; {
    enable = mkBoolOpt false "Whether or not to enable business configuration.";
  };

  config =
    mkIf cfg.enable { nerv = { opt.apps = { frappe-books = on; }; }; };
}
