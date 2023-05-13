{inputs, ...}: {
  /*
  Returns a library color scheme specification.

  Type: String -> { ${n} :: T; colors :: { ${base} :: String }; }
  */
  getColorScheme = name: inputs.nix-colors.colorSchemes.${name};
}
