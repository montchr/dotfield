{self, ...}: let
  inherit (self) inputs lib;
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
    nvfetcher
    sops-nix
    ;

  overlays = [
    agenix.overlay
    gitignore.overlay
    nix-dram.overlay
    nixpkgs-wayland.overlay
    nur.overlay
    nvfetcher.overlay
  ];

  channelsForSystem = system:
    nixpkgs.lib.genAttrs [
      "nixos-stable"
      "nixos-unstable"
      "nixpkgs-trunk"
      "nixpkgs-darwin-stable"
    ] (name: inputs.${name}.legacyPackages.${system});

  packageOverrides = pkgs: let
    inherit (pkgs) system;
    channels = channelsForSystem system;
    # TODO: use these
    # dotfieldPackages = self.packages.${system};
  in {
    inherit channels lib;

    # inherit
    #   (channels.nixos-stable)
    #   zathura
    #   ;

    # inherit
    #   (channels.nixos-unstable)
    #   fish
    #   iosevka
    #   iosevka-bin
    #   iosevka-comfy
    #   kitty
    #   nerdfonts
    #   python3Packages
    #   ;

    # inherit
    #   (channels.nixos-unstable.nodePackages)
    #   pyright
    #   ;

    # # https://github.com/NixOS/nixpkgs/issues/175875
    # inherit
    #   (channels.nixos-unstable.python3Packages)
    #   httpie
    #   ;

    ripgrep = pkgs.ripgrep.override {withPCRE2 = true;};
  };
in {inherit overlays packageOverrides;}
