{
  inputs,
  systems,
  ...
}: let
  l = inputs.nixpkgs.lib // builtins;
in rec {
  isDarwin = l.hasSuffix "-darwin";
  isLinux = l.hasSuffix "-linux";
  eachDarwinSystem = l.genAttrs (l.filter isDarwin systems);
  eachLinuxSystem = l.genAttrs (l.filter isLinux systems);
}
