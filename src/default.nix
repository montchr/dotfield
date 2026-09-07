{ inputs, ... }:
let
  inherit (inputs) import-tree;
in
{
  imports = [
    (import-tree [
      ./lib
      ./features
      ./hosts
      ./modules
      ./overlays
      ./users
    ])
    ./meta
    ./packages
  ];
}
