# -*- mode: sh; eval: (sh-set-shell "bash") -*-
#
# Shell Utilities :: The World
#




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

if [[ -z "$USER" ]]; then
  USER=$(whoami)
fi


# - - - - - - - - - - - - - - - - - - - -
# Functions
# - - - - - - - - - - - - - - - - - - - -

# Get the santized name of the kernel.
function world::get_kernel_name() {
  uname -s | tr '[:upper:]' '[:lower:]'
}

# Get the OS name.
function world::get_os_name() {
  local kernel
  local os_name
  kernel="$(get_kernel_name)"

  [[ "macos" == "${kernel}" ]] && {
    echo "macos"
    return
  }

  [[ "linux" != "${kernel}" ]] && {
    echo "unknown"
    return 1
  }

  os_name=$(
   . /etc/os-release
   printf "%s" "${ID}"
  )

  echo "$os_name"
}

# Get OS version.
function world::get_os_version() {
  local kernel
  local os_name
  kernel="$(get_kernel_name)"

  [[ "macos" == "${kernel}" ]] && {
    sw_vers -productVersion
    return
  }

  [[ "linux" != "${kernel}" ]] \
    && return 1

  version_id=$(
   . /etc/os-release
   printf "%s" "${VERSION_ID}"
  )

  echo "${version_id}"
}

# @TODO needs testing! may not work...
function world::get_os_info() {
  local name=$1
  name="$(echo "$name" | tr '[:upper:]')"
  readonly name

  [[ "linux" != "$(get_kernel_name)" ]] \
    && return 1

  echo "$(
   . /etc/os-release
   printf "%s" "${!name}"
  )"
}


# - - - - - - - - - - - - - - - - - - - -
# The Construct
# - - - - - - - - - - - - - - - - - - - -

KERNEL_NAME="$(world::get_kernel_name)"
OS_NAME="$(world::get_os_name)"
OS_VERSION="$(world::get_os_version)"

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
