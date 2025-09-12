flake@{ self, lib, ... }:
let
  inherit (self.lib) isEmpty;
in
{
  aspects.core = {
    nixos = {
      programs.git.enable = true;
      programs.git.config = {
        safe.directory = [
          # owner should be root:wheel but i cheat and make my own user the owner
          # in some ideal situation, maybe i would deploy the configuration from $XDG_CONFIG_DIR/dotfield
          "/etc/nixos"
          "/etc/dotfield"
        ];
      };
    };

    home =
      {
        config,
        pkgs,
        ...
      }:
      let
        inherit (flake.config.meta.users.${config.home.username}) whoami;
      in
      lib.mkMerge [
        {
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
        (lib.mkIf (!isEmpty (whoami.name or false) && !isEmpty (whoami.email or false)) {
          programs.git = {
            userName = whoami.name;
            userEmail = whoami.email;
          };
        })
      ];
  };
}
