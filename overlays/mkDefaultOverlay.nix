{ inputs }:
final: prev: {
  inherit (inputs.nixos-stable.legacyPackages.${final.system})

    foot
    notmuch
    notmuch-mutt
    ;

  inherit (inputs.nixpkgs-for-beets-not-failing-build.legacyPackages.${final.system})
    beets
    beets-unstable
    beetsPackages
    ;

  # inherit (inputs.nixpkgs-trunk.legacyPackages.${final.system})

  #   ;

}
