{ pkgs, ... }:
{
  imports = [
    ../profiles/multimedia/audio-editing.nix
    ../profiles/multimedia/music/beets/default.nix
  ];

  home.packages = [
    # TODO: what this?
    pkgs.intermodal
  ];
}
