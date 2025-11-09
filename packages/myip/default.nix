{ pkgs, lib, writeShellApplication, ... }:

writeShellApplication {
  name = "myip";

  runtimeInputs = [ pkgs.dnsutils ];

  text = ''
    dig -4 +short @resolver1.opendns.com myip.opendns.com A
    dig -6 +short @resolver1.opendns.com myip.opendns.com AAAA
  '';

  # meta = with lib; {
  #   description = "A dumb tool to get host's current public IP";
  #   homepage = "https://git.azahi.cc/nixfiles";
  #   license = licenses.wtfpl;
  #   maintainers = with maintainers; [ azahi ];
  # };
}
