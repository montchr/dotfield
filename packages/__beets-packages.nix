{
  perSystem =
    { inputs', pkgs, ... }:
    let
      inherit (pkgs) callPackage;
      beets = inputs'.nixpkgs-for-beets-not-failing-build.legacyPackages.beetsPackages.beets-minimal;
    in
    {
      packages = rec {
        beetcamp = callPackage ./beets/plugins/beetcamp.nix {
          inherit beets rich-tables;
        };
        beets-filetote = callPackage ./beets/plugins/filetote.nix {
          inherit beets;
        };
        beet-summarize = callPackage ./beets/plugins/summarize.nix {
          inherit beets;
        };

        rgbxy = callPackage ./python-modules/rgbxy.nix { };
        rich-tables = callPackage ./python-modules/rich-tables.nix {
          inherit rgbxy;
        };
      };
    };
}
