# -*- eval: (sh-set-shell "bash") -*-
# shellcheck shell=bash
#
#
#====\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===>
#::   \\\
#:: ======>        SHELL UTILITIES      ○
#::   ///
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>
#
# This file MUST remain self-contained, with no additional dependencies from
# this repository. The primary use case is the bootstrapping script. Without
# these utilities, the bootstrapping process would be at a disadvantage, so it
# downloads them first thing.
#
# These utilities should be compatible with Bash. ZSH will load this in Bash
# emulation mode for consistency.
#
# Thanks:
# - https://github.com/dylanaraps/pure-bash-bible
#
#
# ============= TABLE OF CONTENTS ============ #
#                                              #
# 1. String Manipulation  ::       [[ str ]]   #
#                                              #
# 2. Messages + Prompts   ::       [[ msg ]]   #
#                                              #
# 3. Shell                ::     [[ shell ]]   #
#                                              #
# 4. Filesystem           ::        [[ fs ]]   #
#                                              #
# 5. Download/Fetch       ::     [[ fetch ]]   #
#                                              #
# 6. Repositories         ::      [[ repo ]]   #
#                                              #
# 7. Guardians            ::     [[ guard ]]   #
#                                              #
# 8. User Management      ::      [[ repo ]]   #
#                                              #
# 9. Package Management   ::       [[ pkg ]]   #
#                                              #
#    9a. Debian/Ubuntu    --  [[ pkg::apt ]]   #
#                                              #
#==============================================#


# shellcheck disable=2153
[[ -n "${ZSH_VERSION}" ]] \
  && emulate -L bash


# Gracefully return if sourcing multiple times.
[[ -n "${UTILS_LOADED}" ]] \
  && return 0
readonly UTILS_LOADED="true"


# Settings
readonly MSG__INDENT="    "  # 4 spaces
readonly MSG__COL__GAP="  "  # 2 spaces


#====\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===>
#:
#:    ==>   STRING MANIPULATION   [[ str ]]
#:
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>


#========================================
# Convert a string to lowercase.
#
# Usage:
#   str::lower <string|@string>
# Parameters:
#   String. Optionally prefixed by `@` for a nameref.
# Outputs:
#   Modified string, unless input is prefixed with `@`.
#========================================
function str::lower {
  ! shell::is_modern && {
    echo "$1" | tr '[:upper:]' '[:lower:]'
    return
  }

  if [[ "@" == "$1"* ]]; then
    local -n str=${1}
    str="${str,,}"
    return
  else
    printf '%s\n' "${1,,}"
  fi
}

#========================================
# Convert a string to uppercase.
#
# Usage:
#   str::upper <string|@string>
# Parameters:
#   String. Optionally prefixed by `@` for a nameref.
# Outputs:
#   Modified string, unless input is prefixed with `@`.
#========================================
function str::upper {
  ! shell::is_modern && {
    echo "$1" | tr '[:lower:]' '[:upper:]'
    return
  }

  if [[ "@" == "$1"* ]]; then
    local -n str=${1}
    str="${str^^}"
    return
  else
    printf '%s\n' "${1^^}"
  fi
}


#========================================
# Strip pattern from start of string.
#
# https://github.com/dylanaraps/pure-sh-bible#strip-pattern-from-start-of-string
#
# Usage:
#   str::lstrip <string> <pattern>
# Parameters:
#   String
#   Pattern
# Outputs:
#   Modified string.
#========================================
function str::lstrip {
  printf '%s\n' "${1##$2}"
}


#========================================
# Strip pattern from end of string.
#
# https://github.com/dylanaraps/pure-sh-bible#strip-pattern-from-end-of-string
#
# Usage:
#   str::rstrip <string> <pattern>
# Parameters:
#   String
#   Pattern
# Outputs:
#   Modified string.
#========================================
function str::rstrip {
    # Usage: rstrip "string" "pattern"
    printf '%s\n' "${1%%$2}"
}


# Sanitize a string, leaving only alphanumeric characters, periods, dashes, and
# underscores.
#
# Parameters:
#   String...
function str::sanitize {
  ! shell::is_modern && {
    msg::error "You need to use a more recent version of Bash!"
    return 1
  }

  local clean="$*"
  clean="${clean//[[:space:]]/\-}"
  clean="${clean//"/"/_}"
  clean="${clean//[^[:word:].-]}"
  str::lower "${clean}"
}

function str::is_supported_version {
  ! shell::is_modern && {
    msg::error "You need to use a more recent version of Bash!"
    return 1
  }

  # shellcheck disable=2206
  declare -a v1=(${1//./ })
  # shellcheck disable=2206
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



#====\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===>
#:
#:    ==>   MESSAGES + PROMPTS    [[ msg ]]
#:
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>


#========================================
# Prompt for text input.
#
# Usage:
#   msg::ask <prompt>
#   msg::ask "What is your name?"
#========================================
function msg::ask {
  msg::question "$1"
  read -r
}

#========================================
# Prompt for input without outputting response feedback.
#
# Usage:
#   msg::ask_silently <prompt>
#   msg::ask_silently "Password: "
# Outputs:
#   STDOUT - Carriage return
#========================================
function msg::ask_silently {
  msg::question "$1"
  read -s -r
  printf "\n"
}

#========================================
# Ask for confirmation and continue immediately upon read.
#
# Usage:
#   msg::ask_for_confirmation <prompt>
#   msg::ask_for_confirmation "Are you sure?"
# Parameters:
#   Message string
# Outputs:
#   STDOUT - Confirmation prompt followed by newline
#========================================
function msg::ask_for_confirmation {
  msg::question "$1 (y/n) "
  read -r -n 1
  printf "\n"
}

#========================================
# Print the user's answer to the most recent prompt.
#
# TODO: Should this use `msg::info`?
#
# Usage:
#   msg::get_answer
# Globals:
#   REPLY
# Outputs:
#   STDOUT - User response text
#========================================
function msg::get_answer {
  printf "%s" "$REPLY"
}

#========================================
# Whether the user responded affirmatively to the most recent prompt.
# 
# Usage:
#   msg::is_confirmed
# Globals:
#   REPLY
# Returns:
#   0 - Affirmative
#   1 - Negative
#========================================
function msg::is_confirmed {
  [[ "$REPLY" =~ ^[Yy]$ ]] &&
    return 0 ||
      return 1
}


#========================================
# Print a header with a short message preceded with its associated domain.
#
# - The line will be preceded with a double-arrow
# - No additional indentation
# - A separator will be output between the domain and message
# - Output will be colorized
# - Line ends with carriage return
#
# Usage:
#   msg::domain <domain> <message>
# Parameters:
#   Domain name
#   Message string
# Outputs:
#   STDOUT - Colorized and formatted domain + message
#========================================
function msg::domain {
  msg::in_green "\n => $1 :: ${*:2}\n\n"
}

#========================================
# Print an indented subheader with a short message.
#
# - Message preceded by indentation
# - Colorized output
# - Line ends with carriage return
#
# Usage:
#   msg::subdomain <message>
# Parameters:
#   Message string
# Globals:
#   MSG__INDENT
# Outputs:
#   STDOUT - Colorized and formatted domain + message
#========================================
function msg::subdomain {
  msg::in_green "\n${MSG__INDENT}${1}\n"
}

#========================================
# Print a de-emphasized domain + message header.
#
# Same features as `msg::domain` with these differences:
# 
# - The line will be preceded with a single-arrow
# - Output will be colorized more subtly
#
# Usage:
#   msg::domain__lesser <domain> <message>
# Parameters:
#   Domain name
#   Message string
# Outputs:
#   STDOUT - Colorized and formatted domain + message
#========================================
function msg::domain__lesser {
  msg::in_purple "\n -> $1 :: ${*:2}\n"
}

#========================================
# Print an "inactive" domain + message header.
#
# Same features as `msg::domain` with these differences:
# 
# - The line will be preceded with a left-facing single-arrow
# - Output will not be colorized
#
# Usage:
#   msg::domain__lesser <domain> <message>
# Parameters:
#   Domain name
#   Message string
# Outputs:
#   STDOUT - Formatted domain + message
#========================================
function msg::domain__inactive {
  msg::print "\n <- $1 :: ${*:2}\n"
}

#========================================
# Print a basic indented informational message.
#
# Usage:
#   msg::success <message>
# Parameters:
#   Message string
# Globals:
#   MSG__INDENT
# Outputs:
#   STDOUT
#========================================
function msg::info {
  msg::print "${MSG__INDENT}${*}"
}

#========================================
# Prompt for a response to a question.
#
# Usage:
#   msg::question <question>
# Parameters:
#   Question string
# Globals:
#   MSG__INDENT
# Outputs:
#   Formatted question prompt to STDOUT
#========================================
function msg::question {
  msg::in_yellow "${MSG__INDENT}[?] $1"
}

#========================================
# Print a message formatted based on a supplied exit code.
#
# Usage:
#   msg::result <exit-code> <message>
#   msg::result 0 "Success!"
#   msg::result 1 "Error!"
#   msg::result $? "some result"
# Parameters:
#   Exit code
#   Message string
# Returns:
#   Exit code provided as argument.
# Outputs:
#   STDOUT
#========================================
function msg::result {
  local code="$1"
  local message="$2"
  if [[ "${code}" -eq 0 ]]; then
    msg::success "${message}"
  else
    msg::error "${message}"
  fi
  return "${code}"
}

#========================================
# Print a green success message to STDOUT.
#
# Usage:
#   msg::success <message>
# Parameters:
#   Message string
# Globals:
#   MSG__INDENT
# Outputs:
#   STDOUT
#========================================
function msg::success {
  msg::in_green "${MSG__INDENT}[✔] $1\n"
}

#========================================
# Print a yellow warning message to STDOUT.
#
# Usage:
#   msg::warning <message>
# Parameters:
#   Message string
# Globals:
#   MSG__INDENT
# Outputs:
#   STDOUT
#========================================
function msg::warning() {
  msg::in_yellow "${MSG__INDENT}[!] $1\n"
}

#========================================
# Print a red error message to STDERR.
#
# TODO: Should only output ANSI color sequences if output to a terminal.
#
# Usage:
#   msg::error <message>
# Parameters:
#   Message string
# Globals:
#   MSG__INDENT
# Outputs:
#   STDERR
#========================================
function msg::error {
  msg::in_red "${MSG__INDENT}[✖] $1\n" >&2
}

#========================================
# Print a stream of messages from input.
#
# Usage:
#   echo "<string>" | msg::stream::info
#   msg::stream::info <HEREDOC>
# Parameters:
#   STDIN
# Outputs:
#   Streams formatted info messages to STDOUT.
#========================================
function msg::stream::info {
  while read -r line; do
    msg::info "${line}"
  done
}

#========================================
# Print a stream of warnings from input.
#
# Usage:
#   echo "<string>" | msg::stream::warnings
#   msg::stream::warnings <HEREDOC>
# Parameters:
#   STDIN
# Outputs:
#   Streams formatted warning messages to STDOUT.
#========================================
function msg::stream::warnings {
  while read -r line; do
    msg::warning "${line}"
  done
}

#========================================
# Print a stream of errors from input.
#
# Usage:
#   echo "<string>" | msg::stream::errors
#   msg::stream::errors <HEREDOC>
# Parameters:
#   STDIN
# Outputs:
#   Streams formatted error message to STDERR.
#========================================
function msg::stream::errors {
  while read -r line; do
    msg::error "$line"
  done
}

#========================================
# Print a stream of messages formatted as a story.
#
# Each message line begins with a distinguishing mark.
#
# Usage:
#   echo "<string>" | msg::stream::story
#   msg::stream::story <HEREDOC>
# Parameters:
#   STDIN
# Outputs:
#   Streams formatted story messages to STDOUT.
#========================================
function msg::stream::story {
  msg::info ""
  msg::info "|"
  while read -r line; do
    msg::info "| ${line}"
  done
  msg::info "|"
  msg::info ""
}

#========================================
# Print a message in green-colored text.
#
# Usage:
#   msg::in_green <message>
# Parameters:
#   Message string
# Outputs:
#   Green text to STDOUT.
#========================================
function msg::in_green {
  msg::in_color "$1" 2
}

#========================================
# Print a message in purple-colored text.
#
# Usage:
#   msg::in_purple <message>
# Parameters:
#   Message string
# Outputs:
#   Purple text to STDOUT.
#========================================
function msg::in_purple {
  msg::in_color "$1" 5
}

#========================================
# Print a message in red-colored text.
#
# Usage:
#   msg::in_red <message>
# Parameters:
#   Message string
# Outputs:
#   Red text to STDOUT.
#========================================
function msg::in_red {
  msg::in_color "$1" 1
}

#========================================
# Print a message in yellow-colored text.
#
# Usage:
#   msg::in_yellow <message>
# Parameters:
#   Message string
# Outputs:
#   Yellow text to STDOUT.
#========================================
function msg::in_yellow {
  msg::in_color "$1" 3
}

#========================================
# Print a message in colorized text.
#
# Usage:
#   msg::in_color <message> <color>
#   msg::in_color "foo bar" 3
# Parameters:
#   Message string
#   Foreground ANSI color code
# Outputs:
#   Colorized text to STDOUT.
#========================================
function msg::in_color {
  local message="$1"
  local color="$2"
  printf "%b" \
    "$(tput setaf "${color}" 2>/dev/null)" \
    "${message}" \
    "$(tput sgr0 2>/dev/null)"
}

#========================================
# Print a message ending with a carriage return.
#
# Usage:
#   msg::print <message>
# Parameters:
#   Message string
# Outputs:
#   Single line of text to STDOUT.
#========================================
function msg::print {
  printf "%b" "$*\n"
}

#========================================
# Print a line of strings, each separated by a fixed amount of whitespace.
#
# In other words, print a "row" of "cells" separated by a "gutter".
#
# Each line ends with a carriage return.
#
# Usage:
#   msg::tabular_row <string>...
#   msg::tabular_row "foo" "bar" "baz"
# Parameters:
#   Multiple short strings
# Outputs:
#   Single line of whitespace-separated strings to STDOUT.
#========================================
function msg::tabular_row {
  for cell in "$@"; do
    printf "%b" "${cell}${MSG__COL__GAP}"
  done
  printf "\n"
}

#========================================
# Print a message accompanied by a spinner.
#
# TODO: improved documentation
#
# Usage:
#   msg::spinner <pid> <cmds> <msg>
#========================================
function msg::spinner {
  local -r PID="$1"
  local -r CMDS="$2"
  local -r MSG="$3"

  local -r FRAMES='/-\|'
  local -r NUMBER_OR_FRAMES=${#FRAMES}
  local i=0
  local frameText=""

  if ! shell::is_ci; then
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
    frameText="${MSG__INDENT}[${FRAMES:i++%NUMBER_OR_FRAMES:1}] $MSG"

    if shell::is_ci; then
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


#====\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===>
#:
#:    ==>  SHELL  [[ shell ]]
#:
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>


function shell::kill_all_subprocesses {
  local i=""
  for i in $(jobs -p); do
    kill "$i"
    wait "$i" &>/dev/null
  done
}

function shell::execute {
  local -r CMDS="$1"
  local -r MSG="${2:-$1}"
  local -r TMP_FILE="$(mktemp /tmp/XXXXX)"
  local exitCode=0
  local cmdsPID=""

  # If the current process is ended, also end all its subprocesses.
  shell::set_trap "EXIT" "shell::kill_all_subprocesses"

  # Execute commands in background
  eval "$CMDS" \
    2>"$TMP_FILE" &

  cmdsPID=$!

  # Show a spinner if the commands require more time to complete.
  msg::spinner "$cmdsPID" "$CMDS" "$MSG"

  # Wait for the commands to no longer be executing in the background, and then
  # get their exit code.
  wait "$cmdsPID" &>/dev/null
  exitCode=$?

  # Print output based on what happened.
  msg::result $exitCode "$MSG"

  if [[ $exitCode -ne 0 ]]; then
    msg::error_stream <"$TMP_FILE"
  fi

  rm -rf "$TMP_FILE"

  return $exitCode
}

# @TODO doesnt work! prob needs a subprocess
function shell::ask_for_sudo {
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


# Whether the current shell is interactive.
# https://www.gnu.org/software/bash/manual/html_node/Is-this-Shell-Interactive_003f.html
function shell::is_interactive {
  [[ $- =~ 'i' ]] && return
  [[ -n "${INTERACTIVE+t}" ]] && return
  [[ -n "${PS1+t}" ]] && return
  return 1
}

# Whether the current shell is run within CI.
function shell::is_ci {
  [[ -n "${CI+t}" ]] && return
  [[ -n "${TRAVIS+t}" ]] && return
  [[ -n "${GITHUB_WORKSPACE+t}" ]] && return
  return 1
}

# Whether the current shell is a recent version.
function shell::is_modern {
  case "${BASH_VERSION}" in
    4*|5*) return 0 ;;
    *)     return 1 ;;
  esac
}

function shell::set_trap {
  trap -p "$1" | grep "$2" &>/dev/null ||
    trap '$2' "$1"
}


#======================================
# Check for the existence of a command in the current shell environment.
#
# Usage:
#   shell::has <command>...
#
# Arguments:
#   Commands...
#========================================
function shell::has {
  for cmd in "$@"; do
    command -v "${cmd}" >/dev/null 2>&1
  done
}


#====\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===>
#:
#:    ==>  FILESYSTEM  [[ fs ]]
#:
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>


#======================================
# Get the basename of a file path.
#
# Alternative to the external `basename`.
#
# https://github.com/dylanaraps/pure-bash-bible#get-the-base-name-of-a-file-path
#
# Usage:
#   fs::basename <path> [suffix]
#
# Arguments:
#   File path.
#   File suffix. Optional.
# Outputs:
#   STDOUT - Basename. Includes file extension unless specifying [suffix].
#========================================
function fs::basename {
  ! shell::is_modern && {
    msg::warning "Bash version 4+ is recommended! Version ${BASH_VERSION} detected."
    msg::warning "Falling back to external 'basename' command..."
    basename "$@"
    return $?
  }

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
#   fs::dirname <path>
# Arguments:
#   File path.
# Outputs:
#   STDOUT - Directory name.
# Returns:
#   0 - If at root directory or no directory.
#========================================
function fs::dirname {
  ! shell::is_modern && {
    msg::warning "Bash version 4+ is recommended! Version ${BASH_VERSION} detected."
    msg::warning "Falling back to external 'dirname' command..."
    dirname "$@"
    return $?
  }

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


#========================================
# Ensure directories exist.
#
# Usage:
#   fs::ensure_dir <dirname>...
# Parameters:
#   Directories...
#========================================
function fs::ensure_dirs() {
  for dir in "$@"; do
    if [[ -d "$dir" ]]; then
      msg::success "$dir"
    else
      msg::info "create: $dir"
      mkdir -p "$dir"
      msg::success "$dir"
    fi
  done
}


#======================================
# Print the name of the owner user of the given file.
#
# On Darwin, favor `gstat` from GNU coreutils, falling back to alternative
# options for use with Darwin's included `/usr/bin/stat`.
#
# https://github.com/dylanaraps/pure-bash-bible#get-the-base-name-of-a-file-path
#
# FIXME: `guard::` functions are only available in bootstrap script!
#
# Usage: fs::get_owner_name <path>
#
# Arguments:
#   Path
# Outputs:
#   STDOUT - Username
# Returns:
#   0 - Success
#   1 - File not found
#========================================
function fs::get_owner_name {
  local t="$1"
  [[ ! -e "$t" ]] && return 1
  if guard::macos; then
    if shell::has gstat; then
      gstat -c '%U' "$t"
    else
      stat -f '%Su' "$t"
    fi
  else
    stat -c '%U' "$t"
  fi
}


#======================================
# Link files from a manifest.
#
# Usage: fs::linkfile <path>
#
# Arguments:
#   Path to a Linkfile manifest
# Outputs:
#   STDOUT - Feedback about link status
#========================================
function fs::linkfile {
  local file="$1"

  msg::subdomain "linkfile: ${file}"

  if [[ -e "${file}" ]]; then
    cd "$(dirname "${file}")" \
      || return 1
    fs::map_lines \
      fs::link \
        "${file}"
  fi
}

# @TODO untested
function fs::link_all {
  local src_dir="$1"
  local target_dir="$2"

  if ! [[ -d "${src_dir}" ]]; then
    msg::error "Source '${src_dir}' is not a directory! Aborting."
    return 1
  elif ! [[ -d "${target_dir}" ]]; then
    msg::error "Target '${target_dir}' is not a directory! Aborting."
    return 1
  fi

  msg::warning "Linking all files in '${src_dir}' to '${target_dir}':"
  for f in "${src_dir}"/*; do
    fs::link "$f" "${target_dir}/$(basename "$f")"
  done
}


#========================================
# Link files or directories safely.
#
# Usage:
#   fs::link <src> <dest>
# Parameters:
#   Source path (relative)
#   Target path (absolute)
#========================================
function fs::link {
  local src_rel_path
  local target_path
  local src_abs_path
  local target_dir
  local owner

  # @TODO is there a better way than eval and an unquoted argument?
  # @TODO is `realpath` available?
  # shellcheck disable=2086
  src_rel_path=$(eval echo $1)
  target_path=$(eval echo "$2")
  readonly \
    src_rel_path \
    target_path

  src_abs_path="$(pwd)/${src_rel_path}"
  target_dir=$(dirname "${target_path}")

  if [[ -d "${target_dir}" ]]; then
    owner="$(fs::get_owner_name "${target_dir}")"
    if [[ "${owner}" != "root" && "${owner}" != "${USER}" ]]; then
      msg::error "can not link '${src_abs_path}' to '${target_path}'"
      msg::error "owner of '${target_dir}' is ${owner}"
      msg::error "allowed owners: root or ${USER}"
      exit 1
    fi
  fi

  if [[ ! -f "${src_abs_path}" && ! -d "${src_abs_path}" ]]; then
    msg::error "can not link '${src_abs_path}' as it does not exist"
    exit 1
  fi

  if [[ ! -d "${target_dir}" ]]; then
    msg::info "create: ${target_dir}"
    mkdir -p "${target_dir}"
  fi

  if [[ -L "${target_path}" ]]
  then

msg::stream::info <<END
┌╴relink: ${src_abs_path}
└───────────╴${target_path}
END

    if [[ "${owner}" = "root" ]]
    then
      sudo rm "${target_path}"
    else
      rm "${target_path}"
    fi

  elif [[ -e "${target_path}" ]]
  then
    msg::warning "Target path '${target_path}' already exists!"
    msg::info "Moving existing path to '${target_path}.bak'..."

    if [[ -e "${target_path}.bak" ]]
    then
      msg::error "Existing path '${target_path}' found, but also found a backup! Aborting."
      return 1
    fi

msg::stream::warnings <<END
┌╴backup: ${target_path}
└───────────╴${target_path}.bak
END

    mv "${target_path}" "${target_path}.bak"

  else

msg::stream::info <<END
┌╴link: ${src_abs_path}
└─────────╴${target_path}
END

  fi

  if [[ "${owner}" = "root" ]]; then
    sudo ln -s "${src_abs_path}" "${target_path}"
  else
    ln -s "${src_abs_path}" "${target_path}"
  fi
}


#========================================
# Concatenate multiple files.
#
# Usage:
#   fs::combine <file>...
# Parameters:
#   Files...
# Outputs:
#   Path to the combined file.
#========================================
function fs::combine {
  local output
  output=$(mktemp)
  for f in "$@"; do
    [[ -f $f ]] \
      && cat "$f" >> "${output}"
  done
  echo "${output}"
}


#========================================
# Invoke a command using every line in a file as arguments.
#
# Usage:
#   fs::map_lines <callback> <file> [<args>...]
# Parameters:
#   Name of callback command.
#   Path to file whose lines contain lists of args.
#   Additional arguments to append to each invocation.
#========================================
function fs::map_lines {
  local callback="$1"
  local file="$2"
  shift 2

  local args="$*"
  local line

  [[ ! -f "${file}" ]] && return 1

  while IFS='' read -r line || [[ -n "${line}" ]]; do
    # Ignore comment lines
    [[ "${line}" == "#"* ]] && continue
    # Pass the entire line as-is -- words will be split for separate args.
    # Strings containing spaces must be quoted.
    # Additional args will be appended.
    # shellcheck disable=2086
    ${callback} ${line} "${args}"
  done < "${file}"
}


#====\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===>
#:
#:    ==>   FETCH/DOWNLOAD  [[ fetch ]]
#:
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>


#========================================
# Download a remote file to the current user's bin directory.
#
# Usage:
#   fetch::to_bin <name> <url>
# Globals:
#   HOME
#   XDG_BIN_HOME
# Parameters:
#   Filename
#   Source URL
#========================================
function fetch::to_bin {
  local name="$1"
  local url="$2"
  local target="${XDG_BIN_HOME:-${HOME}/.local/bin}/${name}"
  fetch::file "${target}" "${url}"
}


#========================================
# Download a remote file and make it executable.
#
# Usage:
#   fetch::file <path> <URL>
# Parameters:
#   Path to target file
#   Source URL
#========================================
function fetch::file {
  local target="$1"
  local url="$2"
  if shell::has curl; then
    curl --silent -o "${target}" "${url}"
  elif shell::has wget; then
    wget -qO "${target}" "${url}" &>/dev/null
  else
    return 1
  fi
  chmod a+x "${target}"
  hash -r
}


#====\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===>
#:
#:    ==>  REPOSITORIES     [[ repo ]]
#:
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>


function repo::is_repo {
  git rev-parse &>/dev/null
}

# Qualify a repo URL.
# Parameters:
#   Repo identifier (e.g. montchr/dotfield) or URL.
#   Forge name or shortname (e.g. github or gh).
function repo::qualify_url {
  local identifier=$1
  local forge=${2:-}

  if [[ "${identifier}" = "https://"* || "${identifier}" = "git@"* ]]; then
    echo "${identifier}"
    return
  fi

  case $forge in
    gh|github)
      if [[ "$USE_HTTPS" = "true" ]]; then
        echo "https://github.com/${identifier}.git"
      else
        echo "git@github.com:${identifier}.git"
      fi
      ;;
    gl|gitlab)
      if [[ "$USE_HTTPS" = "true" ]]; then
        echo  "https://gitlab.com/${identifier}.git"
      else
        echo "git@gitlab.com:${identifier}.git"
      fi
      ;;
    srht|sourcehut)
      if [[ "$USE_HTTPS" = "true" ]]; then
        echo  "https://git.sr.ht/~${identifier}"
      else
        echo "git@git.sr.ht:${identifier}"
      fi
      ;;
  esac
}

function repo::log {
  git --no-pager \
      log \
      --graph \
      --pretty=format:'%Cred%h%Creset %C(bold blue)<%an> -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' \
      "$*"
}

# Get the canonical forge ID.
# Parameters:
#   Forge name
function repo::get_forge_id {
  local forge=$1
  case $forge in
    gh|github) echo "gh" ;;
    gl|gitlab) echo "gl" ;;
    srht|sourcehut) echo "srht" ;;
  esac
}


#========================================
# Get a qualified raw URL for a file in a remote repo.
#
# Usage:
#   repo::qualify_raw_url <forge> <repo_id> <branch> <path>
# Parameters:
#   Forge ID
#   Repo ID
#   Branch
#   Relative path
#========================================
function repo::qualify_raw_url {
  local forge="$1"
  local repo_id="$2"
  local branch="$3"
  local path="$4"

  case $forge in
    gh) : "https://raw.githubusercontent.com/${repo_id}/${branch}/${path}" ;;
    gl) : "https://gitlab.com/${repo_id}/-/raw/${branch}/${path}" ;;
    srht) : "https://git.sr.ht/${repo_id}/blob/${branch}/${path}" ;;
  esac

  printf "%s" "$_"
}


function repo::pluck {
  local forge="$1"
  local repo_id="$2"
  local remote_path="$3"
  local target="$4"
  local branch="${5:-main}"

  local repo_url
  repo_url="$(repo::qualify_raw_url "${forge}" "${repo_id}" "${branch}" "${remote_path}")"
  download::fetch "${target}" "${repo_url}"
}


#========================================
# Synchronize a repository
#
# Usage:
#   repo::sync <target> <forge-id> <repo-id> [<branch>]
#   repo::sync ~/dotfield gh montchr/dotfield
#   repo::sync ~/dotfield gh montchr/dotfield hotfix/foo
# Parameters:
#   Target directory
#   Forge ID -- e.g. gh or github
#   Repo ID -- e.g. montchr/dotfield
#   Branch name -- defaults to `main`
#========================================
function repo::sync {
  local dir=$1
  local forge=$2
  local id=$3
  local branch="${4:-main}"

  local url
  local remote_branch

  msg::subdomain "repo: -> $*"

  forge="$(repo::get_forge_id "${forge}")"
  remote_branch="${forge}/${branch}"

  # @TODO avoid eval
  wd=$(eval echo "${dir}")
  url=$(repo::qualify_url "${id}" "$forge")

  if [[ -d "${wd}/.git" ]]; then
    msg::info "${wd} already exists"
  else
    git clone "${url}" "${wd}" -b "${branch}" --recursive --depth 1
  fi

  cd "${wd}" && {
    git diff-index --quiet HEAD -- || {
      msg::warning "Your working directory is not clean."
      if shell::is_ci; then
        msg::warning "Running via CI workflow. Stashing changes."
        git add . && git stash
      else
        msg::error "Please commit or stash all changes before proceeding."
        return 1
      fi
    }

    current_branch=$(git symbolic-ref --short HEAD)
    if [[ "${branch}" != "${current_branch}" ]]; then
      msg::info "Switching from ${current_branch} to ${branch}"
      git checkout "${branch}"
    fi

    # Note that the remote will be named after the forge, not 'origin'.
    if [[ -d ".git/refs/remotes/${forge}" ]]; then
      current_url=$(git remote get-url "${forge}")
      if [[ "${current_url}" != "${url}" ]]; then
        msg::warning "Remote '${forge}' has wrong url, so updating it"
        msg::warning "  ${current_url} -> ${url}"
        git remote set-url "${forge}" "$url"
      fi
    else
      msg::warning "Could not find remote '${forge}', so adding it"
      git remote add "${forge}" "${url}"
    fi

    msg::info "fetch ${forge}"
    git fetch "${forge}"
    if [[ $(git rev-parse HEAD) == $(git rev-parse "${remote_branch}") ]]; then
      msg::success "Everything up-to-date"
      return 0
    fi

    if [ "$(git rev-list "HEAD..${remote_branch}" --count)" != 0 ]; then
      msg::info "Fetched changes:"
      repo::log "HEAD..${remote_branch}"
      msg::info
    fi

    msg::info "rebase onto ${remote_branch}"
    git rebase "${remote_branch}"

    msg::info "update and reinit submodules"
    git submodule update --init --recursive

    if [[ "${url}" = *"${QUERENT}"* ]]; then
      if [ "$(git rev-list "${remote_branch}..HEAD" --count)" != 0 ]; then
        msg::info "Changes to push:"
        repo::log "${remote_branch}..HEAD"
        msg::info
      fi

      msg::info "pushing changes"
      git push "${forge}" "${branch}"
    fi
  }
}


#====\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===>
#:
#:    ==>  USER MANAGEMENT   [[ user ]]
#:
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>


# Execute a command as a certain user
# Arguments:
#   Account Username
#   Command to be executed
function user::exec () {
  local username=${1}
  local exec_command=${2}
  sudo -u "${username}" -H bash -c "${exec_command}"
}

# Create the new user account.
# Arguments:
#   Account Username
#   Account Password
function user::create_account () {
  local username=${1}
  local password=${2}
  sudo adduser --disabled-password --gecos '' "${username}"
  echo "${username}:${password}" | sudo chpasswd
  sudo usermod -aG sudo "${username}"
}


#========================================
# Whether a user exists.
#
# Usage:
#   user::exists <username>
# Parameters:
#   Username
# Returns:
#   0 - User exists.
#   1 - User does not exist.
#========================================
function user::exists() {
  id "$1" >/dev/null 2>&1 \
    && return 0
  return 1
}


# Set a global $PASSWORD variable by generation or by prompt.
#
# Globals:
#   PASSWORD
function user::set_password_global {
  msg::ask_for_confirmation "Do you want to generate a new password?"
  if msg::is_confirmed; then
    if shell::has bw; then
      PASSWORD=$(bw generate --words 3 --separator '.' -p)
      msg::success "Generated new password:"
      msg::info    "${MSG__INDENT}${PASSWORD}" ; printf "\n"
    else
      msg::error "[Error]" "Couldn't find a password generator!"
    fi
  else
    user::prompt_for_password
  fi
}

# Keep prompting for the password and password confirmation.
#
# Globals:
#   PASSWORD
function user::prompt_for_password {
  local passwords_match=0
  local confirmation
  while [[ "${passwords_match}" -eq "0" ]]; do
    msg::ask_silently "Enter new password:"
    PASSWORD=$(msg::get_answer)

    msg::ask_silently "Confirm password:"
    confirmation=$(msg::get_answer)

    if [[ "${PASSWORD}" != "${confirmation}" ]]; then
      msg::error "Passwords do not match! Please try again."
    else
      passwords_match=1
    fi
  done
}


# Allow passwordless sudo for a user
# Parameters:
#   Username
function user::allow_passwordless_sudo () {
  local username=$1
  sudo cp -v /etc/sudoers /etc/sudoers.bak
  sudo bash -c "echo '${username} ALL=(ALL) NOPASSWD: ALL' | (EDITOR='tee -a' visudo)"
}

#========================================
# Get the home directory for a user.
#
# Usage:
#   user::get_home [<username>]
# Globals:
#   KERNEL_NAME
#   USER
# Parameters:
#   Username. Defaults to current user.
# Outputs:
#   Home directory path inferred from kernel name and username.
#========================================
function user::get_home() {
  local username="${1:-${USER}}"
  case $KERNEL_NAME in
    linux)
      case $username in
        root) echo "/root" ;;
        *) echo "/home/${username}" ;;
      esac
      return
      ;;
    darwin) echo "/Users/${username}" && return ;;
  esac
}


#========================================
# Clone a user's SSH configuration.
#
# Usage:
#   user::clone_ssh <source-user> <target-user>
# Parameters:
#   Source user name
#   Target user name
#========================================
function user::clone_ssh() {
  local source_user=$1
  local target_user=$2
  local source_dir
  local target_dir

  source_dir="$(user::get_home "${source_user}")/.ssh"
  target_dir="$(user::get_home "${target_user}")/.ssh"

  [[ ! -d "$source_dir" ]] && {
    msg::error "[ERROR] Could not locate the SSH directory for '${source_user}'!"
    return 1
  }

  [[ -d $target_dir ]] && {
    msg::warning "The target user already has a '~/.ssh' directory!"
    shell::is_ci || {
        msg::ask_for_confirmation "Are you sure you want to continue?"
        ! msg::is_confirmed && {
          return 1
        }
    }
  }

  sudo cp -rv "${source_dir}" "${target_dir}"
}

#========================================
# Configure SSH for a user.
#
# Usage:
#   user::add_ssh_pub_key <username> [<pubkey>]
# Parameters:
#   Username
#   SSH public key
#========================================
function user::add_ssh_pub_key() {
  local username=${1}
  local pubkey=${2}

  user::exec "${username}" \
    "mkdir -p ~/.ssh; touch ~/.ssh/authorized_keys"

  [[ -n "${pubkey}" ]] && {
    user::exec "${username}" \
      "echo \"${pubkey}\" | sudo tee -a ~/.ssh/authorized_keys"
  }

}

#========================================
# Set the permissions for the user's SSH directory.
#
# Usage:
#   user::set_ssh_permissions <username>
# Parameters:
#   Username
#========================================
function user::set_ssh_permissions() {
  local username=$1
  user::exec "${username}" \
    "chown \"${username}\" ~/.ssh; chmod 700 ~/.ssh; chmod 600 ~/.ssh/authorized_keys"
}

# Modify the sshd_config file.
function user::change_ssh_config () {
  # shellcheck disable=2116
  sudo sed -re \
    's/^(\#?)(PasswordAuthentication)([[:space:]]+)yes/\2\3no/' \
    -i."$(echo 'old')" \
    /etc/ssh/sshd_config
  sudo sed -re \
    's/^(\#?)(PermitRootLogin)([[:space:]]+)(.*)/PermitRootLogin no/' \
    -i /etc/ssh/sshd_config
}


#====\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===>
#:
#:    ==>   PACKAGE MANAGEMENT    [[ pkg ]]
#:
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>


#
# : Debian/Ubuntu :: [[ apt ]]
# - - - - - - - - - - - - - - - - - - -

# Resynchronize the package index files from their sources.
function pkg::apt::update_repos () {
  shell::execute \
    "sudo apt-get update -qqy" \
    "APT (update)"
}

# Install the newest versions of all installed packages.
function pkg::apt::upgrade_all() {
  shell::execute \
    "export DEBIAN_FRONTEND=\"noninteractive\" \
      && sudo apt-get -o Dpkg::Options::=\"--force-confnew\" upgrade -qqy" \
    "APT (upgrade)"
}

# Install a package.
# Parameters:
#   Package description
#   Package name
#   Extra arguments to apt-get
function pkg::apt::install () {
  local DESCRIPTION="$1"
  local PACKAGE="$2"
  local EXTRA_ARGUMENTS="$3"

  if ! pkg::apt::exists "${PACKAGE}"; then
    sudo apt-get install --allow-unauthenticated -qqy "${EXTRA_ARGUMENTS}" "${PACKAGE}"
    if [[ $? ]]; then
      msg::success "${DESCRIPTION}"
    else
      msg::error "${DESCRIPTION}"
    fi
  else
    msg::success "${DESCRIPTION}"
  fi
}

# Whether a package is installed.
# Parameters:
#   Package name
function pkg::apt::exists () {
  dpkg -s "$1" &> /dev/null
}
