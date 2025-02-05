{ lib, ... }:
let
  inherit (lib) recurseIntoAttrs;
in
{
  perSystem =
    { pkgs, ... }:
    let
      inherit (pkgs) callPackages;
    in
    {
      legacyPackages = {
        aporetic = recurseIntoAttrs (callPackages ./aporetic/package.nix { });
      };
    };

}
