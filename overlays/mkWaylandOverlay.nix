{ inputs }:
final: prev: {
  # XXX: Failing build on nixpkgs-wayland
  inherit (inputs.nixos-unstable.legacyPackages.${final.system})
    foot
    waybar
    ;
}
