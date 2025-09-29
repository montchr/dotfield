{ inputs, ... }:
{
  imports = [
    inputs.flake-parts.flakeModules.easyOverlay
  ];

  perSystem =
    { inputs', config, ... }:
    {
      overlayAttrs = {
        inherit (inputs'.nixos-stable.legacyPackages) calibre;
        inherit (inputs'.nixpkgs-trunk.legacyPackages) zellij;
      };
    };
}
