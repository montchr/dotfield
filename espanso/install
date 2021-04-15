#!/usr/bin/env bash
set -euo pipefail

espanso register

plugins=(
  greek-letters-alt
)

for plugin in $plugins; do
  espanso package install "$plugin"
done

espanso restart
