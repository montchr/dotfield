{
  flake,
  pkgs,
  ...
}:
{
  home.packages = [
    pkgs.quodlibet-full

    flake.perSystem.packages.scotty
  ];
}
