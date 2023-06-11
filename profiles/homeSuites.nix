{homeProfiles}: let
  base = [
    homeProfiles.core.default
    homeProfiles.atuin
    homeProfiles.development.nix-tools
    homeProfiles.direnv
    homeProfiles.fzf
    homeProfiles.git
    homeProfiles.navi
    homeProfiles.nnn
    homeProfiles.nvim.default
    homeProfiles.shells.prompts.starship.default
    homeProfiles.shells.fish.default
    homeProfiles.shells.zsh.default
    homeProfiles.ssh
  ];

  remote = base;

  # TODO: dissolve into workstation?
  developer =
    base
    ++ [
      # homeProfiles.difftools.delta
      homeProfiles.difftools.difftastic
      homeProfiles.emacs.default
      homeProfiles.just
      homeProfiles.python
      homeProfiles.zellij
    ];

  graphical = [
    homeProfiles.chromium
    homeProfiles.desktop.common
    homeProfiles.firefox.default
    homeProfiles.keyboard.default
    homeProfiles.kitty.default
    homeProfiles.misc
    homeProfiles.mpv
    homeProfiles.theme.default
    homeProfiles.yt-dlp
    # FIXME: nix-managed preferences don't work well with stateful changes (e.g. font size, theme, etc.)
    # vscode
  ];

  personalised = [
    homeProfiles.apple-music
    homeProfiles.espanso.default
    homeProfiles.misc
    homeProfiles.newsboat
    homeProfiles.obs-studio
    homeProfiles.rclone
    homeProfiles.spotify
    homeProfiles.sync
    homeProfiles.zotero
  ];

  trusted = [
    homeProfiles.gpg
    homeProfiles.secrets.password-store
    homeProfiles.secrets.rbw

    # FIXME: upstream bug? `mu` is hardcoded, not a reference to `pkgs.mu`
    # > Activating runMuInit
    # > /nix/store/crf1jbfp5zs9l4xrpfck5lh4sk5d5rlx-home-manager-generation/activate: line 290: mu: command not found
    # mail
  ];

  webdev = [
    homeProfiles.aws
    homeProfiles.nodejs
    homeProfiles.development.javascript
    homeProfiles.development.php
    homeProfiles.development.wordpress
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
        homeProfiles.vhs
        homeProfiles.yubikey
      ];
  };
in
  roles
