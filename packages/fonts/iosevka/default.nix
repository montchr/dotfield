channels: final: prev: let
  inherit
    (channels.nixos-unstable)
    iosevka
    iosevka-bin
    nerdfonts
    ;
  xtalVariantOverrides = builtins.readFile ./iosevka-xtal-variants.toml;
in {
  # https://github.com/be5invis/Iosevka/blob/master/doc/custom-build.md
  iosevka-xtal = iosevka.override {
    set = "xtal";
    privateBuildPlan = ''
      [buildPlans.iosevka-xtal]
      family = "Iosevka Xtal"
      spacing = "normal"
      serifs = "sans"
      no-cv-ss = false
      # Required for Kitty. See Iosevka's custom build docs.
      export-glyph-names = true

      [buildPlans.iosevka-xtal.variants]
      inherits = "ss08"

      [buildPlans.iosevka-xtal.variants.design]
      ${xtalVariantOverrides}

      [buildPlans.iosevka-xtal.ligations]
      inherits = "dlig"
    '';
  };

  iosevka-xtal-term = iosevka.override {
    set = "xtal-term";
    privateBuildPlan = ''
      [buildPlans.iosevka-xtal-term]
      family = "Iosevka Xtal Term"
      spacing = "term"
      serifs = "sans"
      no-cv-ss = false

      [buildPlans.iosevka-xtal-term.variants]
      inherits = "ss08"

      [buildPlans.iosevka-xtal-term.variants.design]
      ${xtalVariantOverrides}

      [buildPlans.iosevka-xtal-term.ligations]
      inherits = "dlig"

      [buildPlans.iosevka-xtal-term.weights.light]
      shape = 300
      menu = 300
      css = 300

      [buildPlans.iosevka-xtal-term.weights.regular]
      shape = 400
      menu = 400
      css = 400

      [buildPlans.iosevka-xtal-term.weights.medium]
      shape = 500
      menu = 500
      css = 500

      [buildPlans.iosevka-xtal-term.weights.bold]
      shape = 700
      menu = 700
      css = 700

      [buildPlans.iosevka-xtal-term.weights.heavy]
      shape = 900
      menu = 900
      css = 900
    '';
  };

  iosevka-nf = nerdfonts.override {fonts = ["Iosevka"];};

  iosevka-fixed = iosevka-bin.override {variant = "sgr-iosevka-fixed";};
  iosevka-term = iosevka-bin.override {variant = "sgr-iosevka-term";};

  iosevka-slab = iosevka-bin.override {variant = "sgr-iosevka-slab";};
  iosevka-fixed-slab = iosevka-bin.override {variant = "sgr-iosevka-fixed-slab";};
  iosevka-term-slab = iosevka-bin.override {variant = "sgr-iosevka-term-slab";};

  iosevka-curly = iosevka-bin.override {variant = "sgr-iosevka-curly";};
  iosevka-fixed-curly = iosevka-bin.override {variant = "sgr-iosevka-fixed-curly";};
  iosevka-term-curly = iosevka-bin.override {variant = "sgr-iosevka-term-curly";};

  iosevka-curly-slab = iosevka-bin.override {variant = "sgr-iosevka-curly-slab";};
  iosevka-fixed-curly-slab = iosevka-bin.override {variant = "sgr-iosevka-fixed-curly-slab";};
  iosevka-term-curly-slab = iosevka-bin.override {variant = "sgr-iosevka-term-curly-slab";};

  iosevka-aile = iosevka-bin.override {variant = "aile";};
  iosevka-etoile = iosevka-bin.override {variant = "etoile";};
}
