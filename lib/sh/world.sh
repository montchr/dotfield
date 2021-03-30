#!/usr/bin/env bash
#
# Shell Utilities :: The World
#


# - - - - - - - - - - - - - - - - - - - -
# The Querent
# - - - - - - - - - - - - - - - - - - - -

querent="cdom"

if [ -z "$USER" ]; then
  USER=$(whoami)
fi


# - - - - - - - - - - - - - - - - - - - -
# The Construct
# - - - - - - - - - - - - - - - - - - - -

KERNEL_NAME=$(uname -s | tr '[:upper:]' '[:lower:]')
KERNEL_RELEASE=$(uname -r | tr '[:upper:]' '[:lower:]')
OS_NAME="unknown"
OS_VERSION="unknown"
case $KERNEL_NAME in
  darwin)
    OS_NAME=macos
    OS_VERSION=$(sw_vers -productVersion)
    ;;
  linux)
    case $KERNEL_RELEASE in
      *arch*|*coreos*)
        OS_NAME="arch"
        ;;
    esac
    if [[ -e /etc/os-release ]]; then
      OS_VERSION="$(
        . /etc/os-release
        printf "%s" "$VERSION_ID"
      )"
    fi
    ;;
  *)
    ;;
esac


# - - - - - - - - - - - - - - - - - - - -
# Home
# - - - - - - - - - - - - - - - - - - - -

export PATH=$HOME/.local/bin:$PATH

target=$HOME/.config
if [[ -d "$XDG_CONFIG_HOME" ]]; then
  target="$XDG_CONFIG_HOME"
fi
if [[ -d "$GITHUB_WORKSPACE" ]]; then
  target="$GITHUB_WORKSPACE"
fi

export XDG_CONFIG_HOME=$target
export XDG_CONFIG_CACHE="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

export DEVELOPER=$HOME/dev
if [[ "$USER" != "$querent" ]]; then
  export DEVELOPER=$HOME/dev/personal
fi
