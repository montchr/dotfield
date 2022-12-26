{inputs, ...}: let
  l = inputs.nixpkgs.lib // builtins;
in rec {
  /*
  Convert a boolean to a "yes" or "no" string.

  Type: boolToYesNo :: boolean -> string
  */
  boolToYesNo = value:
    if value
    then "yes"
    else "no";

  /*
  Apply a function to the first character in a string.

  Type: transformFirstChar :: (string -> string) -> string -> string

  Example:
    transformFirstChar (s: s + s) "fnord"
    => "ffnord"
    transformFirstChar builtins.toUpper "fnord"
    => "Fnord"
  */
  transformFirstChar = f: s: (f (l.substring 0 1 s)) + l.substring 1 (-1) s;

  /*
  Convert the first character of the given string to lowercase.

  Type: lowerFirstChar :: string -> string

  @partial

  Example:
    lowerFirstChar "MacOS"
    => "macOS"
  */
  lowerFirstChar = transformFirstChar l.toLower;

  /*
  Capitalise the first letter of the given string.

  Type: upperFirstChar :: string -> string

  @partial

  Example:
    upperFirstChar "nixOS"
    => "NixOS"
  */
  upperFirstChar = transformFirstChar l.toUpper;

}
