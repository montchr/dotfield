{
  inputs,
  cell,
}: let
  inherit (inputs) std;
  l = inputs.nixpkgs.lib // builtins;
  pkgs = inputs.nixpkgs;
in
  l.mapAttrs (_: std.lib.dev.mkShell) {
    default = {...}: {
      name = "dotfield";
      imports = [
        std.std.devshellProfiles.default
        cell.devshellProfiles.default
      ];
    };
    ci = _: {
      name = "dotfield-ci";
      packages = with pkgs; [
        cachix
        deadnix
        just
        nvd
        nvfetcher
        shellcheck
        statix
      ];
    };
  }
