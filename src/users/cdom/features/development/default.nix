{
  dotfield.users.cdom.aspects.development.home =
    { config, pkgs, ... }:
    {
      home.shellAliases."d" = "direnv";

      home.packages = [
        pkgs.copier
      ];
    };
}
