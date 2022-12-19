{
  inputs,
  systems,
  ...
}: let
  l = inputs.nixpkgs.lib // builtins;
in rec {
  isDarwin = l.hasSuffix "-darwin";
  isLinux = l.hasSuffix "-linux";
  eachDarwinSystem = attrs: l.genAttrs (l.filter isDarwin) (_: attrs);
  eachLinuxSystem = attrs: l.genAttrs (l.filter isLinux) (_: attrs);
}
