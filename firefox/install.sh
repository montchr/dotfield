#!/usr/bin/env bash
#
# Link Firefox user chrome directory into profiles.
#

set -e

PROFILE_NAME=${1:-.*}

PROFILES_DIR="${HOME}/Library/Application Support/Firefox/Profiles"

link_chrome () {
  [[ -z "$*" ]] && \
    echo "Needs a base directory!" && exit 1

  declare dest src
  src="$(realpath "${XDG_CONFIG_HOME}/firefox/chrome")"
  for profile in "$@"; do
    dest="${profile}/chrome"

    if [[ ( -d $dest ) && ( ! -L $dest ) ]]; then
      echo "Renaming existing directory to '${dest}.bak'..."
      if [[ -d "${dest}.bak" ]]; then
        echo "Existing directory ${dest} found, but also found a backup! Skipping..."
        continue
      fi

      mv "${dest}" "${dest}.bak"
    fi

    ln -sf "$src" "$dest" && \
      ls -al "$dest"
  done
}
export -f link_chrome

fd -t d -d 1 "${PROFILE_NAME}" "${PROFILES_DIR}" -X \
  bash -ic 'link_chrome "$@"' --
