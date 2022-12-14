{inputs, ...}: let
  inherit (inputs.nix-colors) colorSchemes;
  l = inputs.nixpkgs.lib // builtins;

  /*
  Returns a library color scheme specification.

  Type: getColorScheme :: string -> AttrSet
  */
  getColorScheme = name: colorSchemes.${name};

  /*
  Returns a set of prefixed hex color values for a library color scheme.

  Type: getColorScheme :: string -> AttrSet
  */
  getColorScheme' = name: withHexPrefixes (getColorScheme name).colors;

  /*
  Prefix each value in an attrset of strings with a hashmark to represent a set
  of color values.

  Type: withHexPrefixes :: (string, string) -> (string, string)
  @partial
  */
  withHexPrefixes = let
    applyPrefix = _: v:
      if (l.hasPrefix "#" v)
      then v
      else ("#" + v);
  in
    l.mapAttrs applyPrefix;
in {
  inherit
    getColorScheme
    getColorScheme'
    withHexPrefixes
    ;

  inverseSchemeType = type:
    if ("dark" == type)
    then "light"
    else if ("light" == type)
    then "dark"
    else throw "Unsupported color scheme type '${type}' specified.";
}
