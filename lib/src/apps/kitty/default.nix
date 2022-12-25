{
  inputs,
  self,
  ...
}: let
  inherit (self.strings) boolToYesNo;
  inherit (self.colors) withHexPrefixes;
  inherit (l.generators) mkKeyValueDefault toKeyValue;
  l = inputs.nixpkgs.lib // builtins;
  codepoints = import ./codepoints.nix {inherit inputs;};
in {
  inherit codepoints;

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
