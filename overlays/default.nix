{ inputs }:
[
  inputs.emacs-overlay.overlays.default
  inputs.niri.overlays.niri

  (import ./mkDefaultOverlay.nix { inherit inputs; })
  (import ./mkWaylandOverlay.nix { inherit inputs; })
]
