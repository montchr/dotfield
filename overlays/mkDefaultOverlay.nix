{ nixos-stable, nixpkgs-trunk }:
final: prev: {
  inherit (nixos-stable.legacyPackages.${final.system})
    beets
    beets-unstable
    beetsPackages
    ;

  inherit (nixpkgs-trunk.legacyPackages.${final.system})
    # HACK: for compatibility with rust v1.80 <https://github.com/NixOS/nixpkgs/issues/332957>
    nix-init
    ;
}
