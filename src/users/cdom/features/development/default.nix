flake@{ lib, ... }:
let
  inherit (flake.config.dotfield.meta.users.cdom) whoami;
in
{
  dotfield.users.cdom.aspects.development.home =
    { config, pkgs, ... }:
    {
      imports = [ flake.config.dotfield.aspects.development.home ];

      home.shellAliases."d" = "direnv";

      home.packages = [
        pkgs.copier
      ];
    };
}
