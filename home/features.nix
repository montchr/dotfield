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
    homeProfiles.neovim.default
    homeProfiles.nnn
    homeProfiles.rclone
    homeProfiles.shells.zsh.default
    homeProfiles.shells.zsh.with-grml
    homeProfiles.ssh
    homeProfiles.zellij
    homeProfiles.zoxide
  ];

  developer = base ++ [
    homeProfiles.development.difftools.delta
    homeProfiles.emacs.default
    homeProfiles.git-sync
    homeProfiles.just
  ];

  graphical = [
    homeProfiles.graphical.applications.chromium
    homeProfiles.graphical.applications.firefox.default
    homeProfiles.graphical.applications.foot
    homeProfiles.graphical.applications.kitty.default
    homeProfiles.graphical.common
    homeProfiles.graphical.media-client

    homeProfiles.hardware.keyboard.default
    homeProfiles.spotify
    homeProfiles.theme.default
    homeProfiles.yt-dlp
    # FIXME: nix-managed preferences don't work well with stateful changes (e.g. font size, theme, etc.)
    # vscode
  ];

  # TODO: move to user-specific dir -- "personalisation" depends on preference anyway
  personalised = [
    homeProfiles.graphical.applications.obs-studio
    homeProfiles.graphical.espanso
    homeProfiles.rclone
    homeProfiles.spotify
    homeProfiles.sync
  ];

  trusted = [
    homeProfiles.gpg.default
    homeProfiles.password-store
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
        homeProfiles.graphical.applications.kitty.default
        homeProfiles.graphical.applications.okular
        homeProfiles.graphical.applications.xournal
        homeProfiles.development.common
        homeProfiles.development.data-wrangling
        homeProfiles.emacs.default
        homeProfiles.emacs.org-protocol
        homeProfiles.git.repo-manager
        homeProfiles.git.with-pgp-signing
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
