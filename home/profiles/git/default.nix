{
  config,
  lib,
  pkgs,
  flake,
  ...
}:
let
  inherit (flake.perSystem) packages;
  inherit (config.dotfield.whoami) email fullName githubUserName;
in
{
  home.packages = [
    pkgs.difftastic # <- syntax-aware structural diff tool
    pkgs.exiftool # <- EXIF diff handler
    pkgs.hut # <- a sourcehut CLI (unofficial)

    pkgs.gitAndTools.hub
    pkgs.gitAndTools.gh
    pkgs.gitAndTools.tig

    ##: --- for occasional-use ---

    # git-filter-repo # :: history-rewrite toolkit + repo analysis + alternative
    # to `git filter-branch` recommended *in the git manual itself*
  ];

  programs.gh.enable = true;
  programs.gh.settings.git_protocol = "ssh";
  programs.gh.extensions = [
    packages.gh-i
    packages.gh-repo-explore
    packages.gh-s

    pkgs.gh-dash
    pkgs.gh-eco
  ];

  programs.lazygit.enable = true;

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;

    userEmail = email;
    userName = fullName;

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
        };
      }
    ];
  };
}
