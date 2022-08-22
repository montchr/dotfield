{profiles}:
with profiles; let
  developer = [
    dhall
    direnv
    emacs
    git
    python
    shells.zsh
    shells.fish
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

  remote = [
    shells.fish
    ssh
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
