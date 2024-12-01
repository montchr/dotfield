{
  flake,
  pkgs,
  ...
}:
{
  imports = [ ./mpd.nix ];

  home.packages = [
    pkgs.quodlibet-full

    flake.perSystem.packages.scotty
  ];
}
