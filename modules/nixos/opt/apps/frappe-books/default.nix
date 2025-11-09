{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.apps.frappe-books;
in
{
  options.nerv.opt.apps.frappe-books = with types; {
    enable = mkBoolOpt false "Whether or not to enable FrappeBooks.";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ nerv.frappe-books ];
  };
}
