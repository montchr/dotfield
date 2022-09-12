{self, ...}: let
  inherit (self) inputs packages;
  internalLib = self.lib;
  mkChannels = pkgs: (pkgs.lib.genAttrs [
    "nixos-stable"
    "nixos-unstable"
    "nixpkgs-trunk"
    "nixpkgs-darwin-stable"
  ] (name: inputs.${name}.legacyPackages.${pkgs.system}));
in {
  flake.overlays = {
    internalLib = final: prev: {
      lib = prev.lib.extend (lfinal: lprev: {
        ##: eso :: from within
        eso = internalLib;
      });
    };
    # packages = final: prev: (packages.${prev.system});
    overrides = final: prev: let
      channels = mkChannels final;
    in {
      inherit channels;

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

      ripgrep = final.ripgrep.override {withPCRE2 = true;};
    };
  };
}
