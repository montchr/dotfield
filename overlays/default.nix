{
  self,
  withSystem,
  ...
}: let
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
      packagesByInput = withSystem prev.system ({inputs', ...}:
        mapAttrs (_: v: v.packages) {
          inherit
            (inputs')
            agenix
            gitignore
            nil-lsp
            rnix-lsp
            ;
        });
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
        # I find the name `nil` to be pretty confusing especially in the
        # context of Emacs where the literal `nil` symbol is ubiquitous...
        nil-lsp = nil-lsp.nil;
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
            ;
        });
    in {
      inherit channels;

      ripgrep = prev.ripgrep.override {withPCRE2 = true;};
    };
  };
}
