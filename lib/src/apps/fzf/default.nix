{inputs, ...}: let
  l = inputs.nixpkgs.lib // builtins;
in rec {
  makeTheme = {colors}:
    l.concatStringsSep ","
    (l.mapAttrsToList
      (l.generators.mkKeyValueDefault {} ":")
      (makeThemeAttrs {inherit colors;}));

  makeThemeAttrs = {colors}: let
    themeAttrs = import ./makeThemeAttrs.nix {inherit colors;};
    applyPrefix = _: v:
      if (l.hasPrefix "#" v)
      then v
      else ("#" + v);
  in (l.mapAttrs applyPrefix themeAttrs);
}
