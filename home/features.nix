{ profiles }:
let
  base = [
    profiles.core.default
    profiles.atuin
    profiles.development.nix-tools
    profiles.direnv
    profiles.fzf
    profiles.git.default
    profiles.helix
    profiles.navi
    profiles.nnn
    profiles.nvim.default
    profiles.rclone
    profiles.shells.zsh.default
    profiles.shells.zsh.with-grml
    profiles.ssh
    profiles.zellij
    profiles.zoxide
  ];

  developer = base ++ [
    profiles.difftools.delta
    profiles.emacs.default
    profiles.git-sync
    profiles.just
    profiles.python
    profiles.zellij
  ];

  graphical = [
    profiles.chromium
    profiles.desktop.common
    profiles.firefox.default
    profiles.keyboard.default
    profiles.kitty.default
    profiles.foot
    profiles.media-client
    profiles.spotify
    profiles.theme.default
    profiles.yt-dlp
    # FIXME: nix-managed preferences don't work well with stateful changes (e.g. font size, theme, etc.)
    # vscode
  ];

  # TODO: move to user-specific dir -- "personalisation" depends on preference anyway
  personalised = [
    profiles.apple-music
    profiles.espanso.default
    profiles.newsboat
    profiles.obs-studio
    profiles.rclone
    profiles.spotify
    profiles.sync
  ];

  trusted = [
    profiles.gpg.default
    profiles.secrets.password-store
    profiles.secrets.rbw
  ];

  features = {
    inherit
      base
      developer
      graphical
      personalised
      trusted
      ;

    workstation =
      developer
      ++ graphical
      ++ personalised
      ++ trusted
      ++ [
        profiles.desktop.applications.okular
        profiles.desktop.applications.xournal
        profiles.development.common
        profiles.development.data-wrangling
        profiles.emacs.default
        profiles.emacs.org-protocol
        profiles.git.repo-manager
        profiles.git.with-pgp-signing
        profiles.kitty.default
        profiles.ledger
        profiles.pandoc
        profiles.sync
        profiles.vhs
        profiles.writing
        profiles.yubikey
      ];
  };
in
features
