{inputs, ...}: let
  inherit (inputs.apparat.lib) withHexPrefixes;
  /*
  Returns a library color scheme specification.

  Type: String -> { ${n} :: T, ...; colors :: { ${base} :: String }; }
  */
  getColorScheme = name: inputs.nix-colors.colorSchemes.${name};
in {
  inherit getColorScheme;

  /*
  Returns a set of prefixed hex color values for a library color scheme.

  Type: String -> { ${base} :: String }
  */
  getColorScheme' = name: withHexPrefixes (getColorScheme name).colors;
}
