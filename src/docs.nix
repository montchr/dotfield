{
  config,
  lib,
  inputs,
  self,
  ...
}:
{
  perSystem =
    { pkgs, system, ... }:
    let
      # Import all dotfield modules to get complete option set
      dotfieldModules = {
        imports = [
          ./modules/dotfield/default.nix
          ./modules/dotfield/metadata.nix
          ./modules/dotfield/modules.nix
          ./modules/dotfield/nixos.nix
        ];
      };

      # Create a module evaluation to extract all dotfield options
      eval = lib.evalModules {
        modules = [
          dotfieldModules
          {
            _module.args = {
              inherit
                self
                lib
                inputs
                config
                ;
            };
          }
        ];
      };

      # Generate the complete options documentation for all dotfield options
      optionsDoc = pkgs.nixosOptionsDoc {
        options = eval.options.dotfield or { };
        warningsAreErrors = false;
      };

      # Create a comprehensive markdown header
      markdownHeader = pkgs.writeText "dotfield-options-header.md" ''
        # Dotfield Configuration Options

        This document describes all available configuration options under `options.dotfield`.

        The dotfield module system provides:
        - User metadata and preferences (`dotfield.meta.users`)
        - NixOS module definitions (`dotfield.nixos`)
        - Home-manager module definitions (`dotfield.home`)
        - Modular component system (`dotfield.modules`)

        ## Complete Option Reference

      '';

      # Combine header with generated documentation
      completeDoc = pkgs.runCommand "dotfield-complete-options.md" { } ''
        cat ${markdownHeader} > $out
        cat ${optionsDoc.optionsCommonMark} >> $out
      '';
    in
    {
      packages = {
        dotfield-options-doc = completeDoc;
        dotfield-options-json = optionsDoc.optionsJSON;
        dotfield-options-commonmark = optionsDoc.optionsCommonMark;
      };
    };
}
