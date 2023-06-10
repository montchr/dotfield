{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit
    (config.dotfield.whoami)
    email
    fullName
    githubUserName
    pgpPublicKey
    ;

  enableSigning =
    config.programs.gpg.enable
    && config.services.gpg-agent.enable
    && "" != pgpPublicKey;
in {
  home.packages =
    (with pkgs.gitAndTools; [
      hub
      gh
      tig
    ])
    ++ (with pkgs; [
      difftastic #   <- syntax-aware structural diff tool
      exiftool #     <- EXIF diff handler
      git-cliff #    <- flexible changelog generator

      ##: --- for occasional-use ---

      # git-filter-repo # :: history-rewrite toolkit + repo analysis + alternative
      # to `git filter-branch` recommended *in the git manual itself*
    ]);

  programs.lazygit.enable = true;

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;

    userEmail = email;
    userName = fullName;

    signing = lib.mkIf enableSigning {
      key = pgpPublicKey;
      signByDefault = true;
    };

    aliases = {
      snapshot = ''!git stash save "snapshot: $(date)" && git stash apply "stash@{0}"'';
    };

    ignores = [
      # OS or Editor files
      "._*"
      ".DS_Store"
      ".DS_Store?"
      "ehthumbs.db"
      "Thumbs.db"
      ".tern-project"

      # Files that might appear on external disks
      ".Spotlight-V100"
      ".Trashes"

      # Always-ignore extensions
      "*~"
      "*.err"
      "*.orig"
      "*.pyc"
      "*.rej"
      "*.sw?"
      "*.vi"
      "*.bak"

      # Secrets
      "*.pem"
      ".env"
      "auth.json"
    ];

    extraConfig = lib.mkMerge [
      {
        init.defaultBranch = "main";
        github.user = githubUserName;

        # Result: <short-sha> <commit-message> (<pointer-names>) -- <commit-author-name>; <relative-time>
        pretty.nice = "%C(yellow)%h%C(reset) %C(white)%s%C(cyan)%d%C(reset) -- %an; %ar";

        tig = {
          line-graphics = "auto";
        };

        ##: --- remotes ---

        fetch.recurseSubmodules = true;
        push.default = "current";
        apply.whitespace = "nowarn";
        pull.rebase = true;

        ##: --- diff/merge tools ---

        rerere.enabled = true;
        merge.conflictstyle = "diff3";
        merge.tool = "ediff";

        diff = {
          algorithm = "histogram";
          exif.textconv = "${pkgs.exiftool}/bin/exiftool";
          colorMoved = "dimmed-zebra";
          tool = "ediff";
          # `plutil` is a darwin utility
          plist.textconv = "plutil -convert xml1 -o -";
        };
      }
    ];
  };

  programs.gh.enable = true;
  programs.gh.settings.git_protocol = "ssh";
}
