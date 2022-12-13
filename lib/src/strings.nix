{inputs, ...}: let
  l = inputs.nixpkgs.lib // builtins;
in {
  /*
  Convert a boolean to a "yes" or "no" string.

  Type: boolToYesNo :: boolean -> string
  */
  boolToYesNo = value:
    if value
    then "yes"
    else "no";

  /*
  Capitalise the first letter of the given string.

  Type: upperFirstChar :: string -> string

  Example:
    upperFirstChar "nixOS"
    => "NixOS"
  */
  upperFirstChar = s: l.toUpper (l.substring 0 1 s) + l.substring 1 (-1) s;
}
