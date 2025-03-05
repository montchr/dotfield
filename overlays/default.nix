{ inputs }:
[
  inputs.nixpkgs-wayland.overlay
  inputs.emacs-overlay.overlays.default

  (import ./mkDefaultOverlay.nix { inherit inputs; })
  (import ./mkWaylandOverlay.nix { inherit inputs; })
]
