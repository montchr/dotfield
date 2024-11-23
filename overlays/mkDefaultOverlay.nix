{ nixpkgs-beets-pr-358086, nixpkgs-trunk }:
final: prev: {
  # <https://github.com/NixOS/nixpkgs/pull/358086>
  inherit (nixpkgs-beets-pr-358086.legacyPackages.${final.system})
    beets
    beets-unstable
    beetsPackages
    ;

  inherit (nixpkgs-trunk.legacyPackages.${final.system})
    # HACK: for compatibility with rust v1.80 <https://github.com/NixOS/nixpkgs/issues/332957>
    nix-init
    ;
}
