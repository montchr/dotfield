# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

flake@{ inputs, ... }:
let
  inherit (inputs.apparat.lib) isEmpty;
in
{
  aspects.development.home =
    { config, lib, ... }:
    let
      inherit (flake.config.meta.users.${config.home.username}) whoami;
    in
    {
      # https://gist.github.com/thoughtpolice/8f2fd36ae17cd11b8e7bd93a70e31ad6
      programs.jujutsu.settings.revset-aliases = lib.mkMerge [
        {
          "user(x)" = "author(x) | committer(x)";

          "immutable_heads()" = "present(trunk()) | untracked_remote_bookmarks() | tags()";

          "gh_pages()" = "ancestors(remote_bookmarks(exact:\"gh-pages\"))";

          "wip()" = "description(regex:\"^wip\\b\") | description(glob-i:\"wip:*\")";
          "private()" = "description(glob-i:\"private:*\")";
          "blacklist()" = "wip() | private()";

          "closest_pushable(to)" = ''
            heads(::to & mutable() & ~(blacklist() & mutable()):: & ~description(exact:"") & (~empty() | merges()))
          '';

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
        }
        (lib.mkIf (!(isEmpty whoami.email.primary)) {
          "mine()" = lib.concatMapStringsSep " | " (x: "user(\"${x}\")") (lib.attrValues whoami.email);
        })
      ];
    };
}
