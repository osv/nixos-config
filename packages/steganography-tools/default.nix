{ pkgs, lib, writeShellApplication, ... }:

writeShellApplication {
  name = "steg";
  runtimeInputs = with pkgs; [ p7zip steghide coreutils ];
  text = builtins.readFile ./steg.bash;

  # nix run .#steganography-tools hide /tmp/original.jpg /tmp/hidden.jpg /tmp/secret.txt
  meta = with lib; {
    description = "Steganography tool combining 7z encryption with steghide image embedding";
    license = licenses.mit;
    platforms = platforms.linux;
  };
}
