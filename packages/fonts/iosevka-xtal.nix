{iosevka}: let
  makeXtalFamily = set: family: {spacing ? "normal"}: (iosevka.override {
    inherit set;
    # https://github.com/be5invis/Iosevka/blob/master/doc/custom-build.md
    privateBuildPlan = ''
      [buildPlans.iosevka-${set}]
      family = "${family}"
      spacing = "${spacing}"
      serifs = "sans"
      no-cv-ss = false
      # Required for Kitty. See Iosevka's custom build docs.
      export-glyph-names = true

      [buildPlans.iosevka-${set}.variants]
      inherits = "ss08"

      [buildPlans.iosevka-${set}.variants.design]
      eszet = "sulzbacher-descending"
      zero = "slashed-split"
      number-sign = "slanted-open"
      dollar = "interrupted-cap"
      cent = "bar-interrupted"
      percent = "rings-segmented-slash"
      lig-ltgteq = "slanted"

      [buildPlans.iosevka-${set}.ligations]
      inherits = "dlig"
    '';
  });
in {
  xtal = makeXtalFamily "xtal" "Iosevka Xtal" {};
  xtal-term = makeXtalFamily "xtal-term" "Iosevka Xtal Term" {spacing = "term";};
}
