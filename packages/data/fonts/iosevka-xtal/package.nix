{fontWeights}: {
  familySuffix ? "",
  spacing ? "normal",
  weights ? ["thin" "light" "regular" "semibold" "bold" "heavy"],
  slopes ? ["oblique" "italic"],
  serifs ? "sans",
  no-cv-ss ? false,
  export-glyph-names ? true,
}: {
  iosevka,
  lib,
}: let
  set =
    lib.concatStringsSep "-" (["xtal"]
      ++ (lib.splitString " " (lib.toLower familySuffix)));
  maybeSuffix = lib.optionalString (familySuffix != "") " ${familySuffix}";
  extraSlopes = lib.genAttrs slopes (name:
    {angle = 9.4;}
    // (lib.genAttrs ["shape" "menu" "css"]
      (_: name)));
in
  iosevka.override {
    inherit set;
    privateBuildPlan = {
      inherit spacing serifs no-cv-ss export-glyph-names;
      family = "Iosevka Xtal" + maybeSuffix;
      weights = lib.genAttrs weights (name:
        lib.genAttrs ["shape" "menu" "css"]
        (_: fontWeights.${name}));
      ligations.inherits = "dlig";
      variants = {
        inherits = "ss08";
        eszet = "sulzbacher-descending-serifless";
        zero = "slashed-split";
        number-sign = "slanted-open";
        dollar = "interrupted-cap";
        percent = "rings-segmented-slash";
        cent = "open";
      };
      slopes =
        {
          upright = {
            angle = 0;
            shape = "upright";
            menu = "upright";
            css = "normal";
          };
        }
        // extraSlopes;
    };
  }
