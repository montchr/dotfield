flake@{ inputs, lib, ... }:
let
  inherit (inputs.apparat.lib) isEmpty;
in
{
  aspects.core = {
    nixos = {
      programs.git.enable = true;
      programs.git.config = {
        safe.directory = [ "/etc/nixos" ];
      };
    };
  };

  aspects.development.home =
    { config, pkgs, ... }:
    let
      inherit (flake.config.meta.users.${config.home.username}) whoami;
    in
    lib.mkMerge [
      {
        programs.git = {
          enable = true;
          package = pkgs.gitAndTools.gitFull;
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

          aliases = {
            snapshot = ''!git stash save "snapshot: $(date)" && git stash apply "stash@{0}"'';
          };

          extraConfig = {
            init.defaultBranch = lib.mkDefault "main";
            # Result: <short-sha> <commit-message> (<pointer-names>) -- <commit-author-name>; <relative-time>
            pretty.nice = lib.mkDefault "%C(yellow)%h%C(reset) %C(white)%s%C(cyan)%d%C(reset) -- %an; %ar";
            fetch.recurseSubmodules = true;
            push.default = "current";
            merge.conflictstyle = "diff3";
            rerere.enabled = true;
          };
        };

        home.packages = [
          pkgs.codeberg-cli
          pkgs.gitAndTools.hub
          pkgs.gitAndTools.gh
          pkgs.glab
          pkgs.hut # <- a sourcehut CLI (unofficial)
        ];

        programs.gh.enable = true;
        programs.gh.settings.git_protocol = lib.mkDefault "ssh";
      }
      (lib.mkIf (!isEmpty (whoami.name or false)) {
        programs.git.userName = whoami.name;
      })
      (lib.mkIf (!isEmpty (whoami.email.primary or false)) {
        programs.git.userEmail = whoami.email.primary;
      })
      (lib.mkIf (!isEmpty (whoami.accounts.github or false)) {
        programs.git.extraConfig.github.user = whoami.accounts.github;
      })
    ];
}
