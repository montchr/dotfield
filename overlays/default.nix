{
  self,
  withSystem,
  ...
}: let
  inherit (self) inputs;
  inherit (builtins) mapAttrs;
  internalLib = self.lib;
in {
  flake.overlays = {
    internalLib = _final: prev: {
      lib = prev.lib.extend (_lfinal: _lprev: {
        ##: eso :: from within
        eso = internalLib;
      });
    };
    externalPackages = _final: prev: let
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
    overrides = _final: prev: {
      inherit (inputs.nixpkgs-fork-update-iosevka.legacyPackages.${prev.system}) iosevka-bin;
      ripgrep = prev.ripgrep.override {withPCRE2 = true;};
    };
  };
}
