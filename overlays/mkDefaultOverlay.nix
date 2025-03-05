{ inputs }:
final: prev: {
  inherit (inputs.nixos-stable.legacyPackages.${final.system})
    beets
    beets-unstable
    beetsPackages
    notmuch
    notmuch-mutt
    ;

  # inherit (inputs.nixpkgs-trunk.legacyPackages.${final.system})
  #
  #   ;

}
