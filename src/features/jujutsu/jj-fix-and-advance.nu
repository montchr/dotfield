# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
# SPDX-FileCopyrightText: (C) 2023-2026 Jesse Hallett
# SPDX-License-Identifier: GPL-3.0-or-later AND Apache-2.0

# https://github.com/hallettj/home.nix/blob/95a7d4b69adeec62d9b03b893e03b6e7492414c4/modules/features/jujutsu/default.nix
# https://github.com/jj-vcs/jj/issues/405#issuecomment-5416011904

def main [
  ...names: string   # Move bookmarks matching the given name patterns
  --to (-t): string  # Move bookmarks to this revision
] {
  # Mirror the defaults that `jj bookmark advance` would compute internally
  # so that the revset used for `jj fix -s` below matches what would be
  # moved by `jj bookmark advance`.
  let target = $to | default (jj config get revsets.bookmark-advance-to)
  let bookmark = if ($names | is-empty) {
    $"heads\(::\(($target)\) & bookmarks\(\)\)"
  } else {
    let patterns = ($names | each {|n| $"bookmarks\(($n)\)"} | str join " | ")
    $"heads\(::\(($target)\) & \(($patterns)\)\)"
  }

  # Run `fix` if fix tools are configured.
  let added_revs = $"($bookmark)..\(($target)\) & mutable\(\)"
  if (jj config get fix.tools | complete | get exit_code) == 0 {
    jj fix -s $added_revs
  }

  # Run other fixups if run tools are configured.
  if (jj config get run.tools | complete | get exit_code) == 0 {
    # NOTE: `run-all-tools` is a custom alias.
    jj run-all-tools --revision $added_revs
  }

  jj bookmark advance ...$names --to $target
}
