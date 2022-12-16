{
  inputs,
  pkgs,
  ...
}: let
  inherit (nixpkgs-pr-iosevka-bin-update) iosevka-bin;
  nixpkgs-pr-iosevka-bin-update = import inputs.nixpkgs-pr-iosevka-bin-update {
    inherit (pkgs.stdenv.hostPlatform) system;
  };

  makeIosevkaVariant = variant: iosevka-bin.override {inherit variant;};
  makeIosevkaSgrVariant = variant: makeIosevkaVariant "sgr-iosevka-${variant}";
in {
  fonts.fonts = [
    iosevka-bin

    ##: stylistic variants
    (makeIosevkaVariant "aile")
    (makeIosevkaVariant "etoile")
    # pragmatapro style
    (makeIosevkaVariant "ss08")

    ##: width variants
    (makeIosevkaSgrVariant "fixed")
    (makeIosevkaSgrVariant "term")

    ##: slab variants
    (makeIosevkaSgrVariant "slab")
    (makeIosevkaSgrVariant "fixed-slab")
    (makeIosevkaSgrVariant "term-slab")

    ##: curly variants
    (makeIosevkaSgrVariant "curly")
    (makeIosevkaSgrVariant "fixed-curly")
    (makeIosevkaSgrVariant "term-curly")

    ##: curly+slab variants
    (makeIosevkaSgrVariant "curly-slab")
    (makeIosevkaSgrVariant "fixed-curly-slab")
    (makeIosevkaSgrVariant "term-curly-slab")
  ];
}
