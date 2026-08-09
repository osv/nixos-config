{ channels, comma, ... }:

final: prev:

{
  inherit (comma.packages.${final.stdenv.hostPlatform.system}) comma;
}
