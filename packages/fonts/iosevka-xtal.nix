{
  lib,
  iosevka,
}: let
  inherit
    (lib)
    genAttrs
    mapAttrs'
    nameValuePair
    optionalAttrs
    toLower
    ;
  fontWeightDict = {
    thin = 100;
    extraLight = 200;
    light = 300;
    regular = 400;
    medium = 500;
    semibold = 600;
    bold = 700;
    extraBold = 800;
    heavy = 900;
  };
  # Weight names normalised to lowercase.
  fontWeightDict' = mapAttrs' (n: v: nameValuePair (toLower n) v) fontWeightDict;

  makeXtalFamily = set: family: {
    spacing ? "normal",
    withItalic ? true,
    withOblique ? false,
    weights ? ["light" "regular" "medium" "semibold" "bold" "heavy"],
  }: (iosevka.override {
    inherit set;

    # https://github.com/be5invis/Iosevka/blob/master/doc/custom-build.md
    privateBuildPlan = {
      inherit family spacing;
      serifs = "sans";
      no-cv-ss = false;
      # Required for Kitty. See Iosevka's custom build docs.
      export-glyph-names = true;

      weights = genAttrs weights (name: genAttrs ["shape" "menu" "css"] (_: fontWeightDict'.${name}));

      variants.inherits = "ss08";
      variants.design = {
        eszet = "sulzbacher-descending";
        zero = "slashed-split";
        number-sign = "slanted-open";
        dollar = "interrupted-cap";
        cent = "bar-interrupted";
        percent = "rings-segmented-slash";
        lig-ltgteq = "slanted";
      };

      ligations.inherits = "dlig";

      slopes =
        {
          upright = {
            angle = 0;
            shape = "upright";
            menu = "upright";
            css = "normal";
          };
        }
        // (optionalAttrs withItalic {
          italic = {
            angle = 9.4;
            shape = "italic";
            menu = "italic";
            css = "italic";
          };
        })
        // (optionalAttrs withOblique {
          oblique = {
            angle = 9.4;
            shape = "oblique";
            menu = "oblique";
            css = "oblique";
          };
        });
    };
  });
in {
  xtal = makeXtalFamily "xtal" "Iosevka Xtal" {};
  xtal-term = makeXtalFamily "xtal-term" "Iosevka Xtal Term" {spacing = "term";};
}
