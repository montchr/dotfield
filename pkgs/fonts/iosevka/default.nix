final: prev: {
  iosevka-seadome = final.callPackage ./iosevka-seadome.nix {};

  iosevka-fixed = prev.iosevka.override { set = "fixed"; };
  iosevka-term = prev.iosevka.override { set = "term"; };

  iosevka-slab = prev.iosevka.override { set = "slab"; };
  iosevka-fixed-slab = prev.iosevka.override { set = "fixed-slab"; };
  iosevka-term-slab = prev.iosevka.override { set = "term-slab"; };

  iosevka-curly = prev.iosevka.override { set = "curly"; };
  iosevka-fixed-curly = prev.iosevka.override { set = "fixed-curly"; };
  iosevka-term-curly = prev.iosevka.override { set = "term-curly"; };

  iosevka-curly-slab = prev.iosevka.override { set = "curly-slab"; };
  iosevka-fixed-curly-slab = prev.iosevka.override { set = "fixed-curly-slab"; };
  iosevka-term-curly-slab = prev.iosevka.override { set = "term-curly-slab"; };

  iosevka-aile = prev.iosevka.override { set = "aile"; };
  iosevka-etoile = prev.iosevka.override { set = "etoile"; };
  iosevka-sparkle = prev.iosevka.override { set = "sparkle"; };
}
