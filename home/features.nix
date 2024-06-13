let
  developer = [
    # ./profiles/difftools/delta.nix
    # ./profiles/emacs/default.nix
    # ./profiles/git-sync.nix
    # ./profiles/just.nix
    # ./profiles/python.nix
    # ./profiles/zellij.nix
  ];

  graphical = [
    ./profiles/chromium.nix
    ./profiles/desktop/common.nix
    ./profiles/firefox/default.nix
    ./profiles/keyboard/default.nix
    ./profiles/kitty/default.nix
    ./profiles/foot.nix
    ./profiles/media-client.nix
    ./profiles/spotify.nix
    ./profiles/theme/default.nix
    ./profiles/yt-dlp.nix
    # FIXME: nix-managed preferences don't work well with stateful changes (e.g. font size, theme, etc.)
    # vscode
  ];

  # TODO: move to user-specific dir -- "personalisation" depends on preference anyway
  personalised = [
    ./profiles/apple-music.nix
    ./profiles/espanso/default.nix
    ./profiles/newsboat.nix
    ./profiles/obs-studio.nix
    ./profiles/rclone.nix
    ./profiles/spotify.nix
    ./profiles/sync.nix
  ];

  trusted = [
    ./profiles/gpg/default.nix
    ./profiles/secrets/password-store.nix
    ./profiles/secrets/rbw.nix
  ];

  features = {
    inherit
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
        ./profiles/desktop/applications/okular.nix
        ./profiles/desktop/applications/xournal.nix
        ./profiles/development/common.nix
        ./profiles/development/data-wrangling.nix
        ./profiles/emacs/default.nix
        ./profiles/emacs/org-protocol.nix
        ./profiles/git/repo-manager.nix
        ./profiles/git/with-pgp-signing.nix
        ./profiles/kitty/default.nix
        ./profiles/ledger.nix
        ./profiles/pandoc.nix
        ./profiles/sync.nix
        ./profiles/vhs.nix
        ./profiles/writing.nix
        ./profiles/yubikey.nix
      ];
  };
in
features
