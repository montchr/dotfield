{
  perSystem =
    { inputs', pkgs, ... }:
    let
      inherit (pkgs) callPackage;
    in
    {
      packages = rec {
        beetcamp = callPackage ./beets/plugins/beetcamp.nix {
          inherit rich-tables;
          beets = pkgs.beetsPackages.beets-minimal;
        };
        beets-filetote = callPackage ./beets/plugins/filetote.nix {
          beets = pkgs.beetsPackages.beets-minimal;
        };

        rgbxy = callPackage ./python-modules/rgbxy.nix { };
        rich-tables = callPackage ./python-modules/rich-tables.nix {
          inherit rgbxy;
        };
      };
    };
}
