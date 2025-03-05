{ nixos-stable, nixpkgs-trunk }:
final: prev: {
  inherit (nixos-stable.legacyPackages.${final.system})
    beets
    beets-unstable
    beetsPackages
    notmuch
    notmuch-mutt
    ;

  # inherit (nixpkgs-trunk.legacyPackages.${final.system})
  #
  #   ;

}
