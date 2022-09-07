{profiles}:
with profiles; let
  remote = [
    direnv
    git
    shells.fish
    ssh
  ];

  # TODO: dissolve into workstation?
  developer = [
    dhall
    difftools.difftastic
    direnv
    emacs
    git
    python
    shells.fish
    shells.zsh
    ssh
  ];

  graphical = [
    desktop.common
    desktop.gnome
    firefox
    foot
    keyboard
    kitty
    misc
    mpv
    themes
  ];

  personalised = [
    apple-music
    espanso
    misc
    newsboat
    obs-studio
    spotify
    sync
    zotero
  ];

  trusted = [
    gpg
    mail
    promnesia
    secrets.one-password
    secrets.password-store
    yubikey
  ];

  webdev = [
    aws
    nodejs
    development.php
    development.wordpress
  ];

  roles = {
    inherit
      developer
      graphical
      personalised
      remote
      trusted
      webdev
      ;

    workstation =
      developer
      ++ graphical
      ++ personalised
      ++ trusted
      ++ webdev;
  };
in
  roles
