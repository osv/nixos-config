{ lib ? import <nixpkgs/lib> }:

let
  wezterm = import ./default.nix { inherit lib; };
  inherit (wezterm.weztermHelper) normalizeColor;

  # Helper to extract hexToDec for testing
  testHexToDec = hex:
    let
      chars = lib.strings.stringToCharacters (lib.strings.toLower hex);
      charToInt = c:
        let code = lib.strings.charToInt c;
        in if code >= 97 then code - 87 else code - 48;
      accumulate = acc: char: acc * 16 + charToInt char;
    in lib.foldl' accumulate 0 chars;

  # Test cases for hexToDec
  hexTests = {
    "00 should be 0" = testHexToDec "00" == 0;
    "0f should be 15" = testHexToDec "0f" == 15;
    "10 should be 16" = testHexToDec "10" == 16;
    "ff should be 255" = testHexToDec "ff" == 255;
    "FF should be 255" = testHexToDec "FF" == 255;
    "7f should be 127" = testHexToDec "7f" == 127;
    "80 should be 128" = testHexToDec "80" == 128;
    "a5 should be 165" = testHexToDec "a5" == 165;
    "abc should be 2748" = testHexToDec "abc" == 2748;
    "1234 should be 4660" = testHexToDec "1234" == 4660;
  };

  # Test cases for normalizeColor
  colorTests = {
    "regular hex color unchanged" = normalizeColor "#ff0000" == "#ff0000";

    "black with 80% opacity" = normalizeColor "[80]rgb:00/00/00"
      == "rgba(0, 0, 0, 0.8)";

    "white with 100% opacity" = normalizeColor "[100]rgb:ff/ff/ff"
      == "rgba(255, 255, 255, 1)";

    "50% opacity mid-gray" = normalizeColor "[50]rgb:80/80/80"
      == "rgba(128, 128, 128, 0.5)";

    "red with 75% opacity" = normalizeColor "[75]rgb:ff/00/00"
      == "rgba(255, 0, 0, 0.75)";

    "complex color with 20% opacity" = normalizeColor "[20]rgb:a5/b3/c7"
      == "rgba(165, 179, 199, 0.2)";

    "zero opacity" = normalizeColor "[0]rgb:ff/ff/ff"
      == "rgba(255, 255, 255, 0)";
  };

  # Run all tests
  allTests = hexTests // colorTests;

  # Check if all tests pass
  allPass = lib.all (x: x) (lib.attrValues allTests);

  # Get failed tests
  failedTests = lib.filterAttrs (name: result: !result) allTests;

in {
  inherit hexTests colorTests allTests;

  # Summary
  summary = {
    totalTests = lib.length (lib.attrNames allTests);
    passed = lib.length (lib.filter (x: x) (lib.attrValues allTests));
    failed = lib.length (lib.filter (x: !x) (lib.attrValues allTests));
    allPass = allPass;
  };

  # Show results
  results = if allPass then
    "✅ All tests passed!"
  else
    "❌ Failed tests: ${lib.concatStringsSep ", " (lib.attrNames failedTests)}";

  # For debugging: show actual vs expected for failed tests
  debug = failedTests;
}
