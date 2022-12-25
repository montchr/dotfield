{inputs, ...}: let
  l = inputs.nixpkgs.lib // builtins;
in rec {
  prefix = x: "U+" + x;

  /*
  @partial
  */
  range = l.concatMapStringsSep "-" prefix;

  fromVal = x:
    if (l.isString x)
    then (prefix x)
    else (range x);

  /*
  @partial
  */
  string = l.concatMapStringsSep "," fromVal;
}
