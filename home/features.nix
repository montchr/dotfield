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
    # FIXME: what do with these?
    # homeProfiles.shells.prompts.starship.default
    # homeProfiles.shells.prompts.liquidprompt
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
    homeProfiles.misc
    homeProfiles.media-client
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
  ];

  trusted = [
    homeProfiles.gpg
    homeProfiles.secrets.password-store
    homeProfiles.secrets.rbw
  ];

  webdev = [
    # FIXME: aws cli broken in nixpkgs
    # homeProfiles.aws
    homeProfiles.nodejs
    homeProfiles.development.javascript
    homeProfiles.development.php
    homeProfiles.development.wordpress
  ];

  features = {
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
        homeProfiles.desktop.applications.okular
        homeProfiles.desktop.applications.xournal
        homeProfiles.emacs.default
        homeProfiles.emacs.org-protocol
        homeProfiles.git.repo-manager
        homeProfiles.git.with-pgp-signing
        homeProfiles.kitty.default
        homeProfiles.ledger
        homeProfiles.pandoc
        homeProfiles.rclone
        homeProfiles.sync
        homeProfiles.vhs
        homeProfiles.yubikey
      ];
  };
in
features
