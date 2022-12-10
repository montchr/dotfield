{
  inputs,
  lib,
  ...
}: let
  l = inputs.nixpkgs.lib // builtins;
in {
  makeTheme = colors:
    l.concatStringsSep "," (l.mapAttrsToList
      (name: value: "${name}:#${value}")
      colors);
}
