{ pkgs, ... }:
{
  imports = [
    ./common.nix
    ./cliphist.nix
    ./darkman.nix
  ];

  home.packages = with pkgs; [
    dmenu

    # TODO: evaluate
    # lockscreen
    # TODO: <https://gitlab.com/wef/dotfiles/-/blob/master/bin/mylock>
    swaylock
    # bars
    waybar
    # screenshots/capture:
    kooha # screenshot gui
    swappy
    wf-recorder
    # notifications:
    dunst
    mako
    # menus:
    wlogout
    wlr-which-key
    # image viewers:
    pqiv
    swayimg

  ];
}
