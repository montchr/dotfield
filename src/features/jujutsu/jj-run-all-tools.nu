# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
# SPDX-FileCopyrightText: (C) 2023-2026 Jesse Hallett
# SPDX-License-Identifier: GPL-3.0-or-later AND Apache-2.0

# https://github.com/hallettj/home.nix/blob/95a7d4b69adeec62d9b03b893e03b6e7492414c4/modules/features/jujutsu/default.nix
# https://github.com/jj-vcs/jj/issues/405#issuecomment-5416011904

def main [
  --revision (-r): string # Apply tools to specific revisions
] {
  # jj gives inline table syntax, which is invalid at the root document level.
  # Wrap with a key-value assignment to get a valid document.
  let run_tools = $"top = (jj config get run.tools) | from toml | get top"

  for tool in ($run_tools | transpose name entry | sort-by name | get entry) {
    let ignore_changes = $tool | get ignore-changes? | default false
    let heads_only = $tool | get heads-only? | default false
    let revs = if $heads_only { $"heads\(($revision)\)" } else { $revision }

    print --stderr $"jj run -- ($tool.command | str join ' ')"
    jj run --revision $revs ...(if $ignore_changes { [--ignore-changes] } else { [] }) -- ...$tool.command
  }
}
