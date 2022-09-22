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
    externalPackages = final: prev: let
      # NOTE: This must be assigned to a variable instead of performing the
      # system scope change and mapping directly in the resulting attrset in
      # order to prevent infinite recursions.
      packagesByInput =
        withSystem prev.system ({inputs', ...}:
          mapAttrs (_: v: v.packages) {inherit (inputs') agenix gitignore rnix-lsp;});
    in
      with packagesByInput; {
        inherit (agenix) agenix;
        inherit (rnix-lsp) rnix-lsp;
        inherit
          (gitignore)
          gitignoreSource
          gitignoreSourceWith
          gitignoreFilter
          gitignoreFilterWith
          ;
      };
    overrides = final: prev: let
      # Follows the same principle as the `externalPackages` attributes from
      # `inputs'` saved as a variable to prevent infinite recursions.
      channels = withSystem prev.system ({inputs', ...}:
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
