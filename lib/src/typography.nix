{l, ...}: let
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
  fontWeightDict' = l.mapAttrs' (n: v: l.nameValuePair (l.toLower n) v) fontWeightDict;

  withAliases = set:
    set
    // (with fontWeightDict; {
      normal = regular;
      black = heavy;
      demibold = semibold;
    });
in {
  fontWeightValue = name: (withAliases fontWeightDict').name;
  fontWeightName = value: l.findFirst (n: v: v == value) fontWeightDict';
}
