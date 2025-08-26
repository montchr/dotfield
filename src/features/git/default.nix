{
  dotfield.baseline.home =
    { pkgs, ... }:
    {
      programs.git = {
        enable = true;
        package = pkgs.gitAndTools.gitFull;

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
    };
}
