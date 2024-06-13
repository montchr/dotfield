{ pkgs, ... }:
{
  home.packages = [
    pkgs.difftastic # <- syntax-aware structural diff tool
    pkgs.exiftool # <- EXIF diff handler
    pkgs.hut # <- a sourcehut CLI (unofficial)

    pkgs.gitAndTools.hub
    pkgs.gitAndTools.gh

    ##: --- for occasional-use ---

    # git-filter-repo # :: history-rewrite toolkit + repo analysis + alternative
    # to `git filter-branch` recommended *in the git manual itself*
  ];

  programs.gh.enable = true;
  programs.gh.settings.git_protocol = "ssh";

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;

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
      # FIXME: probably should be included in every project repo instead...
      "*.pem"
      ".env"
      "auth.json"
    ];

    extraConfig = {
      init.defaultBranch = "main";

      # Result: <short-sha> <commit-message> (<pointer-names>) -- <commit-author-name>; <relative-time>
      pretty.nice = "%C(yellow)%h%C(reset) %C(white)%s%C(cyan)%d%C(reset) -- %an; %ar";

      ##: remotes
      fetch.recurseSubmodules = true;
      push.default = "current";

      ##: diff/merge tools
      apply.whitespace = "nowarn";
      rerere.enabled = true;
      merge.conflictstyle = "diff3";
    };
  };
}
