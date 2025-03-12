{ inputs }:
final: prev: {
  inherit (inputs.nixos-stable.legacyPackages.${final.system})
    beets
    beets-unstable
    beetsPackages
    foot
    notmuch
    notmuch-mutt
    ;

  # inherit (inputs.nixpkgs-trunk.legacyPackages.${final.system})
  #
  #   ;

}
