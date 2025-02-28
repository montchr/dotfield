{ nixos-unstable, nixpkgs-wayland }:
final: prev: {
  # XXX: Failing build on nixpkgs-wayland
  inherit (nixos-unstable.legacyPackages.${final.system}) waybar;
}
