# -*- mode: sh; eval: (sh-set-shell "bash") -*-
#
#
#====\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===>
#::   \\\
#:: ======>     THE WORLD   ○    [[  shell utilities  ]]
#::   ///
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>
#
#


declare -gx \
  DEVELOPER \
  KERNEL_NAME \
  KERNEL_RELEASE \
  OS_NAME \
  OS_VERSION \
  PATH \
  QUERENT \
  USER \
  XDG_BIN_HOME \
  XDG_CACHE_HOME \
  XDG_CONFIG_CACHE \
  XDG_CONFIG_HOME \
  XDG_DATA_HOME


[[ -z "${USER}" ]] && {
  USER="$(whoami)"
}


[[ -z "$KERNEL_NAME" ]] && {
  KERNEL_NAME="$( uname -s | tr '[:upper:]' '[:lower:]')"
}


[[ -z "$OS_NAME" || -z "$OS_VERSION" ]] && {
  case "${KERNEL_NAME}" in
    darwin)
      OS_NAME="macos"
      OS_VERSION="$(sw_vers -productVersion)"
      ;;
    linux)
      # shellcheck disable=SC1091
      OS_NAME="$(
        . /etc/os-release
        printf "%s" "${ID}"
      )"
      # shellcheck disable=SC1091
      OS_VERSION="$(
        . /etc/os-release
        printf "%s" "${VERSION_ID}"
      )"
      ;;
    *) OS_NAME="unknown" ;;
  esac
}

PATH="$HOME/.local/bin:$PATH"

XDG_CONFIG_HOME="${DOTFIELD:-${HOME}/.config}"
XDG_CONFIG_CACHE="$HOME/.cache"
XDG_DATA_HOME="$HOME/.local/share"
XDG_CACHE_HOME="$HOME/.cache"
XDG_BIN_HOME="${HOME}/.local/bin"

DEVELOPER="${HOME}/Developer"
if [[ "$USER" != "$QUERENT" ]]; then
  DEVELOPER="${HOME}/Developer/99-personal"
fi


function world::info() {

  echo "Kernel name:      $KERNEL_NAME"
  echo "Kernel release:   $KERNEL_RELEASE"
  echo "Operating system: $OS_NAME"
  echo "OS version:       $OS_VERSION"
  echo "User:             $USER"
  echo "XDG_CONFIG_HOME:  $XDG_CONFIG_HOME"
  echo

}
