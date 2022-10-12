{homeProfiles}:
with homeProfiles; let
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
    chromium
    desktop.common
    firefox
    keyboard
    kitty
    misc
    mpv
    themes
    # foot
  ];

  personalised = [
    apple-music
    espanso
    misc
    # FIXME: rust-motd v1.0.0 unavailable? won't build on aarch64-darwin
    # motd
    newsboat
    obs-studio
    spotify
    sync
    zotero
  ];

  trusted = [
    gpg

    # FIXME: upstream bug? `mu` is hardcoded, not a reference to `pkgs.mu`
    # > Activating runMuInit
    # > /nix/store/crf1jbfp5zs9l4xrpfck5lh4sk5d5rlx-home-manager-generation/activate: line 290: mu: command not found
    # mail

    # FIXME: broken
    # promnesia

    secrets.password-store
    yubikey
  ];

  webdev = [
    aws
    nodejs
    development.flyio
    development.javascript
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
