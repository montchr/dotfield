{pkgs, ...}: let
  # FIXME: split this to shared/nixos/darwin-specific
  sharedOverlays = [
    agenix.overlay
    emacs-overlay.overlay
    gitignore.overlay
    nix-dram.overlay
    nixpkgs-wayland.overlay
    nur.overlay
    nvfetcher.overlay
  ];

  channels = {
    nixos-stable = {
      imports = [
        (digga.lib.importOverlays ./overlays/stable)
      ];
    };
    nixpkgs-darwin-stable = {
      imports = [
        (digga.lib.importOverlays ./overlays/stable)
      ];
    };
  };
in {
  nixpkgs = {
    overlays =
      sharedOverlays
      ++ (digga.lib.importOverlays ./overlays/common)
      # FIXME: only for unstable channel -- not for stable
      ++ (digga.lib.importOverlays ./overlays/nixos-unstable)
      ++ [];
    config = {
      allowUnfree = true;
      # https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = pkg: true;

      # TODO: maybe? via https://github.com/Mic92/dotfiles/blob/1b258abff1b69c01c071d5ae2a49cc569ab47235/nixpkgs-config/config.nix#L9
      # pulseaudio = true;
    };
  };
}
