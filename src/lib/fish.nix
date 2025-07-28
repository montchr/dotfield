{ ... }:
{
  toPluginAttrs = pkg: {
    inherit (pkg) src;
    name = pkg.pname;
  };
}
