# -*- mode: sh; eval: (sh-set-shell "bash") -*-
#
# Shell Utilities :: Messages + Prompts
#

[[ ${Utils[msg]} ]] \
  && return \
  || Utils[msg]=${BASH_SOURCE[0]:-${(%):-%x}}

readonly MSG_INDENT=""

# Prompt the user for input.
# Parameters:
#   Prompt message
function msg::ask() {
  msg::question "$1"
  read -r
}

# Prompt the user for input without echoing response.
# Parameters:
#   Prompt message
function msg::ask_silently() {
  msg::question "$1"
  read -s -r
  printf "\n"
}

function msg::ask_for_confirmation() {
  msg::question "$1 (y/n) "
  read -r -n 1
  printf "\n"
}

# Get the user's answer to the previous prompt.
function msg::get_answer() {
  printf "%s" "$REPLY"
}

# Whether the user responded affirmatively to the previous prompt.
# Globals:
#   REPLY
function msg::is_confirmed () {
  [[ "$REPLY" =~ ^[Yy]$ ]] &&
    return 0 ||
      return 1
}

# Print a top-level heading message.
# Deprecated.
# Parameters:
#   Message
function msg::hed () {
  msg::section "$@"
}

# Print a subheading message.
# Parameters:
#   Section name
#   Message
function msg::subhed () {
  msg::subsection "$@"
}

function msg::section() {
  msg::in_green "\n => $1\n"
}

function msg::domain() {
  msg::in_green "\n => $1 :: ${*:2}\n"
}

function msg::domain__lesser() {
  msg::in_purple "\n -> $1 :: ${*:2}\n"
}

function msg::domain__inactive() {
  print "\n <- $1 :: ${*:2}\n"
}

# Print a basic informational message.
# Parameters:
#   Message
function msg::info() {
  print "\n${MSG_INDENT}${1}\n"
}

# Prompt the user for a response to a question.
# Parameters:
#   Message
function msg::question() {
  msg::in_yellow "${MSG_INDENT}[?] $1"
}

# Print a message along with an indication of the result of the previous command.
# Parameters:
#   Result code
#   Message
function msg::result() {
  if [ "$1" -eq 0 ]; then
    msg::success "$2"
  else
    msg::error "$2"
  fi
  return "$1"
}

# Print a message indicating success.
# Parameters:
#   Message
function msg::success() {
  msg::in_green "${MSG_INDENT}[✔] $1\n"
}

# Print a message indicating a warning.
# Parameters:
#   Message
function msg::warning() {
  msg::in_yellow "${MSG_INDENT}[!] $1\n"
}

# Print a message indicating an error.
# Parameters:
#   Message
function msg::error() {
  msg::in_red "${MSG_INDENT}[✖] $*\n"
}

function msg::stream::errors() {
  while read -r line; do
    msg::error "↳ ERROR: $line"
  done
}

function msg::in_green() {
  msg::in_color "$1" 2
}

function msg::in_purple() {
  msg::in_color "$1" 5
}

function msg::in_red() {
  msg::in_color "$1" 1
}

function msg::in_yellow() {
  msg::in_color "$1" 3
}

function msg::in_color() {
  printf "%b" \
    "$(tput setaf "$2" 2>/dev/null)" \
    "$1" \
    "$(tput sgr0 2>/dev/null)"
}

function msg::spinner() {
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
