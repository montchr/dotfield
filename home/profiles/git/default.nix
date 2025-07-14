{
  config,
  pkgs,
  ...
}:
let
  inherit (config.dotfield) whoami;
in
{
  imports = [
    ./__diff.nix
    ./__forge-tools.nix
  ];

  home.packages = [
    pkgs.git-filter-repo
    pkgs.gitAndTools.tig
  ];

  programs.lazygit.enable = true;

  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;

    userEmail = whoami.email;
    userName = whoami.name;

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
    ];

    extraConfig = {
      init.defaultBranch = "main";

      # Result: <short-sha> <commit-message> (<pointer-names>) -- <commit-author-name>; <relative-time>
      pretty.nice = "%C(yellow)%h%C(reset) %C(white)%s%C(cyan)%d%C(reset) -- %an; %ar";

      tig = {
        line-graphics = "auto";
      };

      ## remotes
      fetch.recurseSubmodules = true;
      push.default = "current";
      apply.whitespace = "nowarn";
      pull.rebase = true;
    };
  };
}
