{ pkgs, ... }:
{
  imports = [ ../audio-editing.nix ];

  home.packages = with pkgs; [
    ardour
    carla
    dexed
    mixxx
    puredata
    qsynth
    renoise
    samplv1
    vcv-rack
  ];
}
