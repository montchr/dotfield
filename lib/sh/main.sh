# -*- mode: sh; eval: (sh-set-shell "bash") -*-
#
# Shell Utilities
#

# @TODO ensure proper relative path via dirname
. ./world.sh
. ./msg.sh
. ./fs.sh

# - - - - - - - - - - - - - - - - - - - -
# Commands
# - - - - - - - - - - - - - - - - - - - -


kill_all_subprocesses() {
  local i=""
  for i in $(jobs -p); do
    kill "$i"
    wait "$i" &>/dev/null
  done
}

execute() {
  local -r CMDS="$1"
  local -r MSG="${2:-$1}"
  local -r TMP_FILE="$(mktemp /tmp/XXXXX)"
  local exitCode=0
  local cmdsPID=""

  # If the current process is ended, also end all its subprocesses.
  set_trap "EXIT" "kill_all_subprocesses"

  # Execute commands in background
  eval "$CMDS" \
    &>/dev/null \
    2>"$TMP_FILE" &

  cmdsPID=$!

  if is_interactive && ! is_ci; then
    # Show a spinner if the commands require more time to complete.
    show_spinner "$cmdsPID" "$CMDS" "$MSG"
  fi

  # Wait for the commands to no longer be executing in the background, and then
  # get their exit code.
  wait "$cmdsPID" &>/dev/null
  exitCode=$?

  # Print output based on what happened.
  print_result $exitCode "$MSG"

  if [ $exitCode -ne 0 ]; then
    print_error_stream <"$TMP_FILE"
  fi

  rm -rf "$TMP_FILE"

  return $exitCode
}

ask_for_sudo() {
  # Ask for the administrator password upfront.
  sudo -v &>/dev/null

  # Update existing `sudo` time stamp until this script has finished.
  #
  # https://gist.github.com/cowboy/3118588
  while true; do
    sudo -n true
    sleep 60
    kill -0 "$$" || exit
  done &>/dev/null &
}

get_os() {
  echo "${OS_NAME}"
}

get_os_version() {
  echo "${OS_VERSION}"
}

function get_current_dir () {
    local current_dir="${BASH_SOURCE%/*}"
    if [[ ! -d "${current_dir}" ]]; then current_dir="$PWD"; fi
    echo "${current_dir}"
}

is_git_repository() {
  git rev-parse &>/dev/null
}

is_supported_version() {
  # shellcheck disable=SC2206
  declare -a v1=(${1//./ })
  # shellcheck disable=SC2206
  declare -a v2=(${2//./ })
  local i=""

  # Fill empty positions in v1 with zeros.
  for ((i = ${#v1[@]}; i < ${#v2[@]}; i++)); do
    v1[i]=0
  done

  for ((i = 0; i < ${#v1[@]}; i++)); do
    # Fill empty positions in v2 with zeros.
    if [[ -z ${v2[i]} ]]; then
      v2[i]=0
    fi

    if ((10#${v1[i]} < 10#${v2[i]})); then
      return 1
    elif ((10#${v1[i]} > 10#${v2[i]})); then
      return 0
    fi
  done
}

mkd() {
  if [ -n "$1" ]; then
    if [ -e "$1" ]; then
      if [ ! -d "$1" ]; then
        print_error "$1 - a file with the same name already exists!"
      else
        print_success "$1"
      fi
    else
      execute "mkdir -p $1" "$1"
    fi
  fi
}

# Whether the current shell is interactive.
# https://www.gnu.org/software/bash/manual/html_node/Is-this-Shell-Interactive_003f.html
# Returns:
#   0 = yes
#   1 = no
function is_interactive() {
  [[ -n "$PS1" ]] && return 0
  case "$-" in
    *i*) return 0 ;;
    *)   return 1 ;;
  esac
}

# Whether the current shell is run within CI or Vagrant.
# Returns:
#   0 = yes
#   1 = no
function is_ci() {
  if [[ -n "${VAGRANT}" ]] || [[ -n "${TRAVIS}" ]]; then
    return 0
  else
    return 1
  fi
}

set_trap() {
  trap -p "$1" | grep "$2" &>/dev/null ||
    trap '$2' "$1"
}

show_spinner() {
  local -r PID="$1"
  local -r CMDS="$2"
  local -r MSG="$3"

  local -r FRAMES='/-\|'
  # shellcheck disable=SC2034
  local -r NUMBER_OR_FRAMES=${#FRAMES}
  local i=0
  local frameText=""

  if is_interactive && ! is_ci; then
    # Provide more space so that the text hopefully doesn't reach the bottom
    # line of the terminal window.
    #
    # This is a workaround for escape sequences not tracking the buffer position
    # (accounting for scrolling).
    #
    # See also: https://unix.stackexchange.com/a/278888
    printf "\n\n\n"

    tput cuu 3
    tput sc
  fi

  # Display spinner while the commands are being executed.
  while kill -0 "$PID" &>/dev/null; do
    frameText="   [${FRAMES:i++%NUMBER_OR_FRAMES:1}] $MSG"

    if is_interactive && ! is_ci; then
      printf "%s\n" "$frameText"
      sleep 0.2
      tput rc
    else
      printf "%s" "$frameText"
      sleep 0.2
      printf "\r"
    fi
  done
}
