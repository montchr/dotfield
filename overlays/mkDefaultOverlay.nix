{ inputs }:
final: prev: {
  inherit (inputs.nixos-stable.legacyPackages.${final.system})
    beets
    beets-unstable
    beetsPackages
    ;

  # inherit (inputs.nixpkgs-trunk.legacyPackages.${final.system})
  #
  #   ;

}
