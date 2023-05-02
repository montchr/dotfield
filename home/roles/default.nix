{profiles}:
with profiles; let
  base = [
    core.default
    atuin
    bat
    development.nix-tools
    direnv
    fzf
    git
    # mcfly
    navi
    nnn
    nvim.common
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
      emacs.default
      just
      python
      zellij
    ];

  graphical = [
    chromium
    desktop.common
    firefox.default
    kitty.default
    misc
    mpv
    theme
    vscode
    yt-dlp
    # TODO: empty
    # keyboard
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
