{
  lib,
  pkgs,
  fetchFromGitHub,
  ...
}:
pkgs.stdenv.mkDerivation rec {
  pname = "minimap-font";
  version = "unstable-2021-01-13";

  src = fetchFromGitHub {
    owner = "davestewart";
    repo = "minimap-font";
    rev = "ad96153a6fea125145809f3c460d7c955138f6e3";
    sha256 = "0mx0c4jqkfby1xrnp48b6zmmk96vwy08d48am12qdbkwhgwg20xh";
  };

  installPhase = ''
    mkdir -p $out/share/fonts/truetype
    cp *.ttf $out/share/fonts/truetype/ || cp */*.ttf $out/share/fonts/truetype/ || find . -name "*.ttf" -exec cp {} $out/share/fonts/truetype/ \;
  '';

  meta = with lib; {
    description = "An editor font to give you a 10,000ft view of the code";
    homepage = "https://github.com/davestewart/minimap-font";
    license = licenses.mit;
    platforms = platforms.all;
  };
}