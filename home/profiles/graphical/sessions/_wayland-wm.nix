{
  flake,
  config,
  pkgs,
  ...
}:
let
  prefs = import "${flake.self}/users/${config.home.username}/preferences.nix" {
    inherit pkgs;
  };
in
{
  imports = [
    # A secret service is required.  It's either this one (uses
    # password-store) or GNOME Keyring, but only one can be enabled at
    # a time (they provide mutually-exclusive implementations of the
    # XDG Secret Service protocol).  It's probably not the best idea
    # to make pass-secret-service a requirement, since it requires the
    # user to have configured the password-store properly.
    ../../password-store.nix

    ../common.nix
    ../cliphist.nix
    ../darkman.nix
    ../dunst.nix
    ../eww.nix
    ../kanshi.nix
    #    ../mako.nix
    ../swayidle.nix
    ../swaylock.nix
    ../waybar/default.nix

    ../applications/flameshot.nix

    # FIXME: how to integrate with preferences?  using prefs value in
    # import would result in infinite recursion due to prefs import
    # dependency on username.  regardless, even if it did work, that
    # approach would not scale -- e.g. if 'launcher' pref uses an
    # argument to specify a drun mode (n/a for fuzzel, which is
    # conveniently my current preferred launcher), then the attr can
    # no longer map to a file.
    ../applications/file-managers/nemo.nix
    ../launchers/fuzzel.nix
  ];

  dconf.settings."org/nemo/desktop" = {
    show-desktop-icons = false;
  };

  home.packages = with pkgs; [
    ## Color picker:
    hyprpicker

    ## File manager:
    superfile
    yazi

    # TODO: evaluate
    ## Lockscreen:
    # TODO: <https://gitlab.com/wef/dotfiles/-/blob/master/bin/mylock>
    swaylock

    ## Screenshots/capture/annotation:
    grim
    kooha # screen recorder gui
    satty # annotator
    slurp
    wf-recorder # screen recorder

    ## Menus:
    wlogout
    wlr-which-key

    ## Document viewers:
    kdePackages.koko
    pix
    pqiv # or imv
    swayimg
    zathura

    ## Wallpaper:
    swaybg
    waypaper
  ];
}
