{ pkgs, ... }:
{
  imports = [
    ./common.nix
    ./cliphist.nix
    ./darkman.nix
  ];

  home.packages = with pkgs; [
    # TODO: evaluate
    dunst
    kooha # screenshot gui
    mako
    swappy
    swayimg
    wf-recorder
  ];
}
