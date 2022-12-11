{profiles}:
with profiles; let
  base = [
    direnv
    git
    shells.zsh
    ssh
  ];
  remote = base;

  # TODO: dissolve into workstation?
  developer =
    base
    ++ [
      difftools.delta
      emacs
      python
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
    vscode
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
    secrets.password-store

    # FIXME: upstream bug? `mu` is hardcoded, not a reference to `pkgs.mu`
    # > Activating runMuInit
    # > /nix/store/crf1jbfp5zs9l4xrpfck5lh4sk5d5rlx-home-manager-generation/activate: line 290: mu: command not found
    # mail
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
      base
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
      ++ webdev
      ++ [
        vhs
        yubikey
      ];
  };
in
  roles
