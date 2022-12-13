{
  inputs,
  self,
  ...
}: let
  inherit (self.strings) boolToYesNo;
  inherit (l.generators) mkKeyValueDefault toKeyValue;
  l = inputs.nixpkgs.lib // builtins;
in {
  /*
  Reshape a Base16 color scheme into attrs of hex color strings describing a
  kitty theme.

  Type: makeThemeAttrs :: AttrSet -> AttrSet
  */
  makeThemeAttrs = {colors}: let
    themeAttrs = import ./makeThemeAttrs.nix {inherit colors;};
    applyPrefix = _: v:
      if (l.hasPrefix "#" v)
      then v
      else ("#" + v);
  in (l.mapAttrs applyPrefix themeAttrs);

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
