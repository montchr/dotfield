# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
# SPDX-FileCopyrightText: (C) 2023-2026 Jesse Hallett
# SPDX-License-Identifier: GPL-3.0-or-later AND Apache-2.0

# https://github.com/hallettj/home.nix/blob/95a7d4b69adeec62d9b03b893e03b6e7492414c4/modules/features/jujutsu/default.nix
# https://github.com/jj-vcs/jj/issues/405#issuecomment-5416011904

{
  aspects.development.home =
    {
      lib,
      config,
      pkgs,
      ...
    }:
    let
      cfg = config.programs.jujutsu;
    in
    {
      programs.jujutsu.settings.aliases = {
        ba = [ "fix-and-advance" ];
        fix-and-advance =
          let
            impl = pkgs.writeNushellApplication {
              name = "jj-fix-and-advance";
              runtimeInputs = [ cfg.package ];
              text = builtins.readFile ./jj-fix-and-advance.nu;
            };
          in
          {
            doc = "Same as `jj bookmark advance`, but also runs `jj fix` on revisions added to bookmark history";
            definition = [
              "util"
              "exec"
              "--"
              (lib.getExe impl)
            ];
          };
      };

      programs.jjui.settings = {
        actions = [
          {
            name = "jj-fix-and-advance";
            lua = /* lua */ ''
              jj_async "fix-and-advance"
              jjui.bookmarks.cancel()
              revisions.refresh()
            '';
          }
        ];
        bindings = [
          {
            action = "jj-fix-and-advance";
            key = "a";
            scope = "bookmarks";
            desc = "advance bookmark after fix";
          }
        ];
      };
    };
}
