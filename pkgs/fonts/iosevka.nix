final: prev: {
  iosevka-seadome = final.iosevka.override {
    set = "seadome";
    privateBuildPlan = {
      family = "Iosevka Seadome";
      spacing = "normal";
      serifs = "sans";
      variants = {
        inherits = "ss08";
      };
    };
  };

  iosevka-nf = final.nerdfonts.override { fonts = ["Iosevka"]; };

  iosevka-fixed = final.iosevka-bin.override { variant = "sgr-iosevka-fixed"; };
  iosevka-term = final.iosevka-bin.override { variant = "sgr-iosevka-term"; };

  iosevka-slab = final.iosevka-bin.override { variant = "sgr-iosevka-slab"; };
  iosevka-fixed-slab = final.iosevka-bin.override { variant = "sgr-iosevka-fixed-slab"; };
  iosevka-term-slab = final.iosevka-bin.override { variant = "sgr-iosevka-term-slab"; };

  iosevka-curly = final.iosevka-bin.override { variant = "sgr-iosevka-curly"; };
  iosevka-fixed-curly = final.iosevka-bin.override { variant = "sgr-iosevka-fixed-curly"; };
  iosevka-term-curly = final.iosevka-bin.override { variant = "sgr-iosevka-term-curly"; };

  iosevka-curly-slab = final.iosevka-bin.override { variant = "sgr-iosevka-curly-slab"; };
  iosevka-fixed-curly-slab = final.iosevka-bin.override { variant = "sgr-iosevka-fixed-curly-slab"; };
  iosevka-term-curly-slab = final.iosevka-bin.override { variant = "sgr-iosevka-term-curly-slab"; };

  iosevka-aile = final.iosevka-bin.override { variant = "aile"; };
  iosevka-etoile = final.iosevka-bin.override { variant = "etoile"; };
}
