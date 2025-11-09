{ boomer, ... }:

final: prev: {
  boomer = boomer.packages.${final.system}.boomer.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      ./aa-flashpoint.patch # https://github.com/tsoding/boomer/pull/118
    ];
  });
}
