#!/usr/bin/env bash
#
# utils
#
# Thanks:
# - https://github.com/dylanaraps/pure-bash-bible


# Change a string to lowercase.
# Parameters:
#   String
function string::lower() {
  printf '%s\n' "${1,,}"
}

# Change a string to uppercase.
# Parameters:
#   String
function string::upper() {
    printf '%s\n' "${1^^}"
}

# Sanitize a string, leaving only alphanumeric characters, periods, dashes, and
# underscores.
#
# Parameters:
#   String...
function string::sanitize() {
  local clean="$*"
  clean="${clean//[[:space:]]/\-}"
  clean="${clean//"/"/_}"
  clean="${clean//[^[:word:].-]}"
  string::lower "${clean}"
}


# - - - - - - - - - - - - - - - - - - - -
# Messages + Prompts
# - - - - - - - - - - - - - - - - - - - -

# Prompt the user for input.
# Parameters:
#   Prompt message
ask() {
  print_question "$1"
  read -r
}

# Prompt the user for input without echoing response.
# Parameters:
#   Prompt message
ask_silently() {
  print_question "$1"
  read -s -r
  printf "\n"
}

ask_for_confirmation() {
  print_question "$1 (y/n) "
  read -r -n 1
  printf "\n"
}

# Get the user's answer to the previous prompt.
get_answer() {
  printf "%s" "$REPLY"
}

# Whether the user responded affirmatively to the previous prompt.
# Globals:
#   REPLY
function user_confirmed () {
  [[ "$REPLY" =~ ^[Yy]$ ]] &&
    return 0 ||
      return 1
}

# Set a global $PASSWORD variable by generation or by prompt.
#
# Globals:
#   PASSWORD
function set_password_global() {
  ask_for_confirmation "Do you want to generate a new password?"
  if user_confirmed; then
    if cmd_exists bw; then
      PASSWORD=$(bw generate --words 3 --separator '.' -p)
      print_success "Generated new password:"
      print_info    "    ${PASSWORD}" ; printf "\n"
    else
      print_error "[Error]" "Couldn't find a password generator!"
    fi
  else
    .prompt_for_password
  fi
}

# Keep prompting for the password and password confirmation.
#
# Globals:
#   PASSWORD
function prompt_for_password() {
  local passwords_match=0
  local confirmation
  while [ "${passwords_match}" -eq "0" ]; do
    ask_silently "Enter new password:"
    PASSWORD=$(get_answer)

    ask_silently "Confirm password:"
    confirmation=$(get_answer)

    if [[ "${PASSWORD}" != "${confirmation}" ]]; then
      print_error "Passwords do not match! Please try again."
    else
      passwords_match=1
    fi
  done
}

# Print a top-level heading message.
# Parameters:
#   Message
function print_hed () {
  print_in_purple "\n • $1\n"
}

# Print a subheading message.
# Parameters:
#   Message
function print_subhed () {
  print_in_green "\n   $1\n"
}

# Print a basic informational message.
# Parameters:
#   Message
function print_info() {
  print_in_purple "\n   $1\n"
}

# Prompt the user for a response to a question.
# Parameters:
#   Message
print_question() {
  print_in_yellow "   [?] $1"
}

# Print a message along with an indication of the result of the previous command.
# Parameters:
#   Result code
#   Message
print_result() {
  if [ "$1" -eq 0 ]; then
    print_success "$2"
  else
    print_error "$2"
  fi
  return "$1"
}

# Print a message indicating success.
# Parameters:
#   Message
print_success() {
  print_in_green "   [✔] $1\n"
}

# Print a message indicating a warning.
# Parameters:
#   Message
print_warning() {
  print_in_yellow "   [!] $1\n"
}

# Print a message indicating an error.
# Parameters:
#   Label
#   Message
print_error() {
  print_in_red "   [✖] $1 $2\n"
}

print_error_stream() {
  while read -r line; do
    print_error "↳ ERROR: $line"
  done
}

print_in_green() {
  print_in_color "$1" 2
}

print_in_purple() {
  print_in_color "$1" 5
}

print_in_red() {
  print_in_color "$1" 1
}

print_in_yellow() {
  print_in_color "$1" 3
}

print_in_color() {
  printf "%b" \
    "$(tput setaf "$2" 2>/dev/null)" \
    "$1" \
    "$(tput sgr0 2>/dev/null)"
}


# - - - - - - - - - - - - - - - - - - - -
# Commands
# - - - - - - - - - - - - - - - - - - - -

cmd_exists() {
  command -v "$1" &>/dev/null
}

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

  # Show a spinner if the commands require more time to complete.
  show_spinner "$cmdsPID" "$CMDS" "$MSG"

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
  local os=""
  local kernelName=""
  kernelName="$(uname -s)"

  if [ "$kernelName" == "Darwin" ]; then
    os="macos"
  elif [ "$kernelName" == "Linux" ] && [ -e "/etc/os-release" ]; then
    os="$(
      . /etc/os-release
      printf "%s" "$ID"
    )"
  else
    os="$kernelName"
  fi

  printf "%s" "$os"
}

get_os_version() {
  local os=""
  local version=""

  os="$(get_os)"

  if [ "$os" == "macos" ]; then
    version="$(sw_vers -productVersion)"
  elif [ -e "/etc/os-release" ]; then
    version="$(
      . /etc/os-release
      printf "%s" "$VERSION_ID"
    )"
  fi

  printf "%s" "$version"
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


#======================================
# Get the basename of a file path.
#
# Alternative to the external `basename`.
#
# https://github.com/dylanaraps/pure-bash-bible#get-the-base-name-of-a-file-path
#
# Usage:
#   basename <path> [suffix]
#
# Arguments:
#   File path.
#   File suffix. Optional.
# Outputs:
#   STDOUT - Basename. Includes file extension unless specifying [suffix].
#========================================
basename() {
    local tmp
    tmp=${1%"${1##*[!/]}"}
    tmp=${tmp##*/}
    tmp=${tmp%"${2/"$tmp"}"}
    printf '%s\n' "${tmp:-/}"
}


#========================================
# Get the directory name of a file path.
#
# https://github.com/dylanaraps/pure-bash-bible#get-the-directory-name-of-a-file-path
#
# Alternative to the external `dirname`.
#
# Usage:
#   dirname <path>
# Arguments:
#   File path.
# Outputs:
#   STDOUT - Directory name.
# Returns:
#   0 - If at root directory or no directory.
#========================================
function dirname() {
    local tmp=${1:-.}

    [[ $tmp != *[!/]* ]] && {
        printf '/\n'
        return
    }

    tmp=${tmp%%"${tmp##*[!/]}"}

    [[ $tmp != */* ]] && {
        printf '.\n'
        return
    }

    tmp=${tmp%/*}
    tmp=${tmp%%"${tmp##*[!/]}"}

    printf '%s\n' "${tmp:-/}"
}

# Whether the current shell is interactive.
# https://www.gnu.org/software/bash/manual/html_node/Is-this-Shell-Interactive_003f.html
function is_interactive() {
  [[ $- =~ 'i' ]] && return
  local vars=(INTERACTIVE PS1)
  for var in "${vars[@]}"; do
    [[ -v "${var}" ]] && return
  done
  return 1
}

# Whether the current shell is run within CI or Vagrant.
function is_ci() {
  local vars=(CI TRAVIS VAGRANT)
  for var in "${vars[@]}"; do
    [[ -v "${var}" ]] && return
  done
  return 1
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

  if ! is_ci; then
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

    if is_ci; then
      printf "%s" "$frameText"
      sleep 0.2
      printf "\r"
    else
      printf "%s\n" "$frameText"
      sleep 0.2
      tput rc
    fi
  done
}
