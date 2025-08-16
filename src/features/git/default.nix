{
  dotfield.baseline.home.programs.git = {
    enable = true;

    aliases = {
      snapshot = ''!git stash save "snapshot: $(date)" && git stash apply "stash@{0}"'';
    };

    extraConfig = {
      fetch.recurseSubmodules = true;
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

  };

  dotfield.features.development.home =
    { pkgs, ... }:
    {
      programs.git.package = pkgs.gitAndTools.gitFull;
    };

}
