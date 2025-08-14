{ moduleWithSystem, ... }:
{
  dotfield.features.development.home = moduleWithSystem (
    perSystem@{ packages }:
    home@{ config, pkgs, ... }:
    {
      home.packages = [
        pkgs.codeberg-cli
        pkgs.gitAndTools.hub
        pkgs.gitAndTools.gh
        pkgs.glab
        pkgs.hut # <- a sourcehut CLI (unofficial)
      ];
      programs.gh.enable = true;
    }
  );
}
