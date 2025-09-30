{
  imports = [
    ./__beets-packages.nix
    ./__by-name.nix
  ];

  perSystem =
    { pkgs, system, ... }:
    {
      packages = {
        difftastic-16k = pkgs.difftastic.overrideAttrs (oldAttrs: {
          JEMALLOC_SYS_WITH_LG_PAGE = "16";
        });
      };
    };
}
