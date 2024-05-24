{ homeProfiles }:
let
  base = [
    homeProfiles.core.default
    homeProfiles.atuin
    homeProfiles.development.nix-tools
    homeProfiles.direnv
    homeProfiles.fzf
    homeProfiles.git.default
    homeProfiles.helix
    homeProfiles.navi
    homeProfiles.nnn
    homeProfiles.nvim.default
    homeProfiles.rclone
    homeProfiles.shells.zsh.default
    homeProfiles.shells.zsh.with-grml
    homeProfiles.ssh
    homeProfiles.zellij
    homeProfiles.zoxide
  ];

  developer = base ++ [
    homeProfiles.difftools.delta
    homeProfiles.emacs.default
    homeProfiles.git-sync
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
    homeProfiles.foot
    homeProfiles.media-client
    homeProfiles.spotify
    homeProfiles.theme.default
    homeProfiles.yt-dlp
    # FIXME: nix-managed preferences don't work well with stateful changes (e.g. font size, theme, etc.)
    # vscode
  ];

  # TODO: move to user-specific dir -- "personalisation" depends on preference anyway
  personalised = [
    homeProfiles.apple-music
    homeProfiles.espanso.default
    homeProfiles.newsboat
    homeProfiles.obs-studio
    homeProfiles.rclone
    homeProfiles.spotify
    homeProfiles.sync
  ];

  trusted = [
    homeProfiles.gpg.default
    homeProfiles.secrets.password-store
    homeProfiles.secrets.rbw
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
        homeProfiles.desktop.applications.okular
        homeProfiles.desktop.applications.xournal
        homeProfiles.development.common
        homeProfiles.development.data-wrangling
        homeProfiles.emacs.default
        homeProfiles.emacs.org-protocol
        homeProfiles.git.repo-manager
        homeProfiles.git.with-pgp-signing
        homeProfiles.kitty.default
        homeProfiles.ledger
        homeProfiles.pandoc
        homeProfiles.sync
        homeProfiles.vhs
        homeProfiles.writing
        homeProfiles.yubikey
      ];
  };
in
features
