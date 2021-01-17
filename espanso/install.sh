#!/usr/bin/env zsh
set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

brew bundle --file "${DIR}/Brewfile"

espanso register
