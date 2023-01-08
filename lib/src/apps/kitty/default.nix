{
  l,
  lib,
  ...
}: let
  inherit (lib.colors) withHexPrefixes;
  inherit (lib.strings) boolToYesNo;
  inherit (l.generators) mkKeyValueDefault toKeyValue;
in rec {
  codepoints = import ./codepoints.nix {inherit l;};

  /*
  @partial
  */
  makeSymbolsMap = k: mkKeyValueDefault {} " " (codepoints.string k);

  /*
  Reshape a Base16 color scheme into attrs of hex color strings describing a
  kitty theme.

  Type: makeThemeAttrs :: AttrSet -> AttrSet
  */
  makeThemeAttrs = args: withHexPrefixes (import ./makeThemeAttrs.nix args);

  /*
  Generate a kitty configuration string from an attrset.

  Type: makeConf :: AttrSet -> string
  @partial
  */
  makeConf = toKeyValue {
    listsAsDuplicateKeys = true;
    mkKeyValue = k: v: let
      v' =
        if l.isBool v
        then boolToYesNo v
        else v;
    in
      mkKeyValueDefault {} " " k v';
  };
}
