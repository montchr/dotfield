# -*- mode: sh; eval: (sh-set-shell "bash") -*-
#
# Shell Utilities :: The World
#


[[ ${Utils[world]} ]] \
  && return \
  || Utils[world]=${BASH_SOURCE[0]:-${(%):-%x}}


. ./msg.sh


declare -gx \
  DEVELOPER \
  KERNEL_NAME \
  KERNEL_RELEASE \
  OS_NAME \
  OS_VERSION \
  QUERENT \
  USER \
  PATH \
  XDG_CACHE_HOME \
  XDG_CONFIG_CACHE \
  XDG_CONFIG_HOME \
  XDG_DATA_HOME


# - - - - - - - - - - - - - - - - - - - -
# The Querent
# - - - - - - - - - - - - - - - - - - - -

QUERENT="cdom"

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

PATH=$HOME/.local/bin:$PATH

target=$HOME/.config
if [[ -d "$XDG_CONFIG_HOME" ]]; then
  target="$XDG_CONFIG_HOME"
fi
if [[ -d "$GITHUB_WORKSPACE" ]]; then
  target="$GITHUB_WORKSPACE"
fi

XDG_CONFIG_HOME=$target
XDG_CONFIG_CACHE="$HOME/.cache"
XDG_DATA_HOME="$HOME/.local/share"
XDG_CACHE_HOME="$HOME/.cache"

DEVELOPER=$HOME/dev
if [[ "$USER" != "$QUERENT" ]]; then
  DEVELOPER=$HOME/dev/personal
fi

# - - - - - - - - - - - - - - - - - - - -
# Main
# - - - - - - - - - - - - - - - - - - - -

function world::info() {

  msg::info "Kernel name:      $KERNEL_NAME"
  msg::info "Kernel release:   $KERNEL_RELEASE"
  msg::info "Operating system: $OS_NAME"
  msg::info "OS version:       $OS_VERSION"
  msg::info "User:             $USER"
  msg::info "XDG_CONFIG_HOME:  $XDG_CONFIG_HOME"
  msg::info

}
