# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

{ moduleWithSystem, ... }:
{
  aspects.workstation.home = moduleWithSystem (
    perSystem@{ inputs' }:
    { pkgs, ... }:
    {
      home.packages = [
        perSystem.inputs'.llm-agents.packages.claude-desktop

        # TODO: The package variant should probably vary depending on
        # the host-specific hardware.
        pkgs.ollama
      ];
    }
  );

  aspects.development.home = moduleWithSystem (
    perSystem@{ inputs' }:
    {
      programs.claude-code = {
        enable = true;
        package = perSystem.inputs'.llm-agents.packages.claude-code;
      };
    }
  );
}
