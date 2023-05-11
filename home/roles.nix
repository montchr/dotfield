{profiles}: let
  base = [
    profiles.core.default
    profiles.atuin
    profiles.git
    profiles.editors.nvim
    profiles.shells.prompts.starship
    profiles.shells.zsh
    profiles.ssh
    # FIXME: rust-motd v1.0.0 unavailable? won't build on aarch64-darwin
    # profiles.motd
  ];

  remote = base;

  dev =
    base
    ++ [
      profiles.dev.common
      profiles.dev.difftools.delta
      profiles.dev.langs.nix
      profiles.editors.emacs
      profiles.zellij
      # FIXME: use difftastic but not in emacs / dumb / non-interactive shells
      # profiles.dev.difftools.difftastic
    ];

  gui = [
    profiles.desktop.chromium
    profiles.desktop.common
    profiles.desktop.firefox
    profiles.desktop.kitty
    profiles.media.mpv
    profiles.media.yt-dlp
    profiles.theme
    # FIXME: nix-managed preferences don't work well with stateful changes (e.g. font size, theme, etc.)
    # profiles.editors.vscode
  ];

  personalised = [
    profiles.desktop.espanso

    profiles.media.apple-music
    profiles.media.newsboat
    profiles.media.obs-studio
    profiles.media.spotify
    profiles.media.zotero

    profiles.rclone
    profiles.syncthing
  ];

  trusted = [
    profiles.gpg
    profiles.secrets.bitwarden
    profiles.secrets.password-store

    # FIXME: upstream bug? `mu` is hardcoded, not a reference to `pkgs.mu`
    # > Activating runMuInit
    # > /nix/store/crf1jbfp5zs9l4xrpfck5lh4sk5d5rlx-home-manager-generation/activate: line 290: mu: command not found
    # profiles.mail
  ];

  roles = {
    inherit base dev gui personalised remote trusted;

    workstation =
      dev
      ++ gui
      ++ personalised
      ++ trusted
      ++ [
        profiles.dev.langs.nodejs
        profiles.dev.langs.php
        profiles.ops.aws
        profiles.yubikey
        profiles.vhs
      ];
  };
in
  roles
