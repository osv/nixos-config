{ pkgs, lib, writeShellApplication, ... }:

writeShellApplication {
  name = "steg";
  runtimeInputs = with pkgs; [ p7zip steghide coreutils ];
  text = builtins.readFile ./steg.bash;

  meta = with lib; {
    description = "Steganography tool combining 7z encryption with steghide image embedding";
    license = licenses.mit;
    platforms = platforms.linux;
  };
}
