{homeProfiles}: let
  base = [
    homeProfiles.core.default
    homeProfiles.atuin
    homeProfiles.development.nix-tools
    homeProfiles.direnv
    homeProfiles.fzf
    homeProfiles.git
    homeProfiles.helix
    homeProfiles.navi
    homeProfiles.nnn
    homeProfiles.nvim.default
    homeProfiles.rclone
    # homeProfiles.shells.prompts.starship.default
    homeProfiles.shells.prompts.liquidprompt
    homeProfiles.shells.zsh.default
    homeProfiles.ssh
    homeProfiles.zellij
    homeProfiles.zoxide
  ];

  # TODO: dissolve into workstation?
  developer =
    base
    ++ [
      homeProfiles.difftools.delta
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

  # TODO: move to user-specific dir -- "personalisation" depends on preference anyway
  personalised = [
    homeProfiles.apple-music
    homeProfiles.espanso.default
    homeProfiles.misc
    homeProfiles.newsboat
    homeProfiles.obs-studio
    homeProfiles.rclone
    homeProfiles.spotify
    homeProfiles.sync
    # FIXME: conflicts with iosevka-xtal font profile
    # homeProfiles.theme.fonts.monospace.jetbrains-mono
    homeProfiles.theme.fonts.sans-serif.inter
    homeProfiles.theme.fonts.serif.ibm-plex-serif
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
