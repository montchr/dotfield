{ flake, pkgs, ... }:
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
    ./dunst.nix
    ./kanshi.nix
    ./swayidle.nix
    ./swaylock.nix

    ./applications/nemo.nix
  ];

  dconf.settings."org/nemo/desktop" = {
    show-desktop-icons = false;
  };

  home.packages = with pkgs; [
    ## Launchers:
    bemenu
    fuzzel
    ulauncher

    ## Color picker:
    # TODO: find a non-hypr alternative!
    hyprpicker

    # TODO: evaluate
    ## Lockscreen:
    # TODO: <https://gitlab.com/wef/dotfiles/-/blob/master/bin/mylock>
    swaylock

    ## Bars:
    eww
    flake.perSystem.inputs'.nixos-unstable.legacyPackages.waybar

    ## Screenshots/capture/annotation:
    grim
    kooha # screenshot gui
    satty # annotator
    slurp
    wf-recorder # screen recorder

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
