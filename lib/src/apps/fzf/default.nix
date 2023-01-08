{
  l,
  lself,
  ...
}: let
  inherit (lself.colors) withHexPrefixes;
  inherit (l.generators) mkKeyValueDefault;
in rec {
  /*
  Convert a Base16 color scheme set into a string format suitable for setting
  fzf's `--color` option.

  Type: makeTheme :: AttrSet -> string
  */
  makeTheme = args: (l.concatStringsSep ","
    (l.mapAttrsToList
      (mkKeyValueDefault {} ":")
      (makeThemeAttrs args)));

  /*
  Reshape a Base16 color scheme into attrs of hex color strings describing a
  fzf theme.

  Type: makeThemeAttrs :: AttrSet -> AttrSet
  */
  makeThemeAttrs = args: withHexPrefixes (import ./makeThemeAttrs.nix args);
}
