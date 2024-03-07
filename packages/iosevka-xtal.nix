{ self, ... }:
let
  inherit (self.inputs) apparat;
  inherit (apparat.lib.typography) fontWeights;
in
{
  perSystem =
    { pkgs, ... }:
    let
      inherit (pkgs) callPackages;
      iosevka-xtal = import ./data/fonts/iosevka-xtal/packages.nix { inherit fontWeights; };
    in
    {
      packages = callPackages iosevka-xtal { };
    };
}
