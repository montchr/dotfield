{ pkgs, ... }:
{
  imports = [
    # A secret service is required.  It's either this one (uses
    # password-store) or GNOME Keyring, but only one can be enabled at
    # a time (they provide mutually-exclusive implementations of the
    # XDG Secret Service protocol).  It's probably not the best idea
    # to make pass-secret-service a requirement, since it requires the
    # user to have configured the password-store properly.
    ../password-store.nix

    ./common.nix
    ./cliphist.nix
    ./darkman.nix
    ./mako.nix
    ./swayidle.nix
    ./swaylock.nix
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

    # wallpaper:
    waypaper
  ];
}
