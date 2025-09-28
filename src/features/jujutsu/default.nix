flake@{
  inputs,
  lib,
  self,
  ...
}:
let
  inherit (inputs.apparat.lib) isEmpty;
in
{
  aspects.development.home =
    {
      pkgs,
      config,
      ...
    }:
    let
      inherit (flake.config.meta.users.${config.home.username}) whoami;
    in
    {
      imports = [
        self.modules.homeManager.jujutsu-signing
      ];

      home.packages = [
        pkgs.jjui
      ];

      # This should be, for now, the developer's responsibility.  It is not
      # on individual projects to add an ignore for somebody's exotic
      # workflow until that exotic workflow becomes widely adopted.  Or do
      # you think that adding this to a project's official gitignore is good
      # publicity for Jujutsu?
      programs.git.ignores = [ ".jj*" ];

      programs.jujutsu = {
        enable = true;
        settings = {
          user = {
            name = whoami.name or "";
            email = whoami.email.primary or "";
          };

          ui = {
            log-synthetic-elided-nodes = lib.mkDefault true;
            # For interoperability with other tools that don't know jujutsu.
            conflict-marker-style = "git";
            diff-formatter = ":git";
          };

          git = {
            private-commits = "blacklist()";
            write-change-id-header = true;
          };

          snapshot.auto-update-stale = true;

          aliases = {
            cat = [
              "file"
              "show"
            ];

            credit = [
              "file"
              "annotate"
            ];

            # SYNOPSIS: jj eject <revset>
            # The inverse of harvest.
            "eject" = [
              "squash"
              "--from"
              "@"
              "--into"
            ];

            # SYNOPSIS: jj harvest <revset>
            # The inverse of eject.
            "harvest" = [
              "squash"
              "--into"
              "@"
              "--from"
            ];

            init = [
              "util"
              "exec"
              "--"
              "bash"
              "-c"
              ''
                jj git init --colocate
                jj bookmark track 'glob:*@origin'
              ''
            ];

            open = [
              "log"
              "-r"
              "open()"
            ];

            retrunk = [
              "rebase"
              "-d"
              "trunk()"
            ];

            streamline = [ "simplify-parents" ];

            # Find the closest ancestor with a bookmark pointing at it,
            # and move it to the parent of the working copy.
            tug = [
              "bookmark"
              "move"
              "--from"
              "heads(::@- & bookmarks())"
              "--to"
              "@-"
            ];
          };

          # https://gist.github.com/thoughtpolice/8f2fd36ae17cd11b8e7bd93a70e31ad6
          revset-aliases = {
            "user(x)" = "author(x) | committer(x)";
            "mine()" = lib.concatMapStringsSep " | " (x: "user(\"${x}\")") (lib.attrValues whoami.email);

            "immutable_heads()" = "present(trunk()) | untracked_remote_bookmarks() | tags()";

            "gh_pages()" = "ancestors(remote_bookmarks(exact:\"gh-pages\"))";

            "wip()" = "description(glob:\"wip:*\")";
            "private()" = "description(glob:\"private:*\")";
            "blacklist()" = "wip() | private()";

            # stack(x, n) :: the set of mutable commits reachable from
            #                'x', with 'n' parents.
            #
            # n :: customize the display and return set for certain operations
            # x :: target the set of 'roots' to traverse
            "stack()" = "stack(@)";
            "stack(x)" = "stack(x, 2)";
            "stack(x, n)" = "ancestors(reachable(x, mutable()), n)";

            # open() :: all stacks that are reachable from the working
            #           copy, or any other commit written by the user.
            #           n = 1 excludes everything from `trunk()`, so all
            #           resulting commits are mutable by definition.
            "open()" = "stack(mine() | @, 1)";

            "ready()" = "open() - descendants(blacklist())";
          };
        };
      };
    };
}
