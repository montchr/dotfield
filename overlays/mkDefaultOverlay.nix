{ inputs }:
final: prev: {
  inherit (inputs.nixos-stable.legacyPackages.${final.system})
    beets
    beets-unstable
    beetsPackages
    foot
    ;

  # inherit (inputs.nixpkgs-trunk.legacyPackages.${final.system})
  #
  #   ;

}
