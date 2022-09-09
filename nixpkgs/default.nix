{self, ...}: let
  inherit (self) inputs overlays;
  inherit
    (inputs)
    nixpkgs
    nixpkgs-wayland
    nixos-stable
    nixos-unstable
    darwin
    agenix
    gitignore
    home-manager
    nix-dram
    nur
    sops-nix
    ;

  externalOverlays = [
    agenix.overlay
    gitignore.overlay
    nix-dram.overlay
    nixpkgs-wayland.overlay
    nur.overlay
  ];

  channelsForSystem = system:
    nixpkgs.lib.genAttrs [
      "nixos-stable"
      "nixos-unstable"
      "nixpkgs-trunk"
      "nixpkgs-darwin-stable"
    ] (name: inputs.${name}.legacyPackages.${system});
in {
  flake.nixpkgsConfig.config.nixpkgs = {
    overlays =
      externalOverlays
      ++ [
        # nvfetcher sources must be loaded before other internal packages.
        overlays.sources
        overlays.default
      ];
    config = {
      packageOverrides = pkgs: let
        inherit (pkgs) system;
        channels = channelsForSystem system;
      in {
        inherit channels;

        lib = pkgs.lib.extend (lfinal: lprev: {
          ##: eso :: from within
          eso = self.lib;
        });

        inherit
          (channels.nixos-stable)
          zathura
          ;

        inherit
          (channels.nixos-unstable)
          fish
          iosevka
          iosevka-bin
          iosevka-comfy
          kitty
          nerdfonts
          python3Packages
          ;

        inherit
          (channels.nixos-unstable.nodePackages)
          pyright
          ;

        # https://github.com/NixOS/nixpkgs/issues/175875
        inherit
          (channels.nixos-unstable.python3Packages)
          httpie
          ;

        ripgrep = pkgs.ripgrep.override {withPCRE2 = true;};
      };
      allowUnfree = true;
    };
  };
}
