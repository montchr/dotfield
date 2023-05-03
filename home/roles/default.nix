{profiles}:
with profiles; let
  base = [
    core
    atuin
    development.nix-tools
    direnv
    fzf
    git
    # mcfly
    navi
    nnn
    nvim
    shells.prompts.starship
    shells.zsh
    ssh
  ];

  remote = base;

  # TODO: dissolve into workstation?
  developer =
    base
    ++ [
      # shells.bash.blesh
      # difftools.delta
      difftools.difftastic
      emacs
      just
      python
      zellij
    ];

  graphical = [
    chromium
    desktop.common
    firefox
    keyboard
    kitty
    misc
    mpv
    theme
    vscode
    yt-dlp
  ];

  personalised = [
    apple-music
    espanso
    # FIXME: gross
    misc
    # FIXME: rust-motd v1.0.0 unavailable? won't build on aarch64-darwin
    # motd
    newsboat
    obs-studio
    rclone
    spotify
    sync
    zotero
  ];

  trusted = [
    gpg
    secrets.password-store
    secrets.rbw

    # FIXME: upstream bug? `mu` is hardcoded, not a reference to `pkgs.mu`
    # > Activating runMuInit
    # > /nix/store/crf1jbfp5zs9l4xrpfck5lh4sk5d5rlx-home-manager-generation/activate: line 290: mu: command not found
    # mail
  ];

  webdev = [
    aws
    nodejs
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
        # FIXME: fails to build on aarch64-darwin due to chromium dependency
        # vhs
        yubikey
      ];
  };
in
  roles
