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
    ## Launchers:
    bemenu
    fuzzel
    tofi

    ## Color picker:
    # TODO: find a non-hypr alternative!
    hyprpicker

    # TODO: evaluate
    ## Lockscreen:
    # TODO: <https://gitlab.com/wef/dotfiles/-/blob/master/bin/mylock>
    swaylock

    ## Bars:
    eww
    waybar

    ## Screenshots/capture/annotation:
    kooha # screenshot gui
    swappy
    wf-recorder

    ## Notifications:
    dunst
    mako

    ## Menus:
    wlogout
    wlr-which-key

    ## Document viewers:
    swayimg
    zathura

    ## Wallpaper:
    waypaper
  ];
}
