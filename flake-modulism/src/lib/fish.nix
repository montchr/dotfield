let
  toPluginAttrs = pkg: {
    inherit (pkg) src;
    name = pkg.pname;
  };
in
{
  flake.lib.fish = { inherit toPluginAttrs; };
}
