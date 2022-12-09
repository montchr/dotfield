{inputs, ...}: let
  inherit (inputs.nixpkgs.lib.generators) toKeyValue;
  inherit (inputs.home-manager.lib.hm.booleans) yesNo;
  l = inputs.nixpkgs.lib // builtins;

  makeFontFeatures = name: features: "font_features ${name} ${l.concatStringsSep " " features}";
in {
  inherit makeFontFeatures;

  makeFontFeatures' = family: styles: features:
    l.concatMapStringsSep "\n"
    (style: makeFontFeatures "${family}-${style}" features)
    styles;

  makeTheme = import ./makeTheme.nix {inherit l;};

  makeConf = toKeyValue {
    mkKeyValue = key: value: let
      value' =
        if l.isBool value
        then (yesNo value)
        else l.toString value;
    in "${key} ${value'}";
  };
}
