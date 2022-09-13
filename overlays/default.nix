{
  self,
  withSystem,
  ...
}: let
  inherit (self) inputs packages;
  inherit (builtins) mapAttrs;
  internalLib = self.lib;
in {
  flake.overlays = {
    internalLib = final: prev: {
      lib = prev.lib.extend (lfinal: lprev: {
        ##: eso :: from within
        eso = internalLib;
      });
    };
    overrides = final: prev: let
      channels = withSystem final.system ({inputs', ...}:
        mapAttrs (_: v: v.legacyPackages) {
          inherit
            (inputs')
            nixos-stable
            nixos-unstable
            nixpkgs-trunk
            nixpkgs-darwin-stable
            ;
        });
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

      ripgrep = prev.ripgrep.override {withPCRE2 = true;};
    };
  };
}
