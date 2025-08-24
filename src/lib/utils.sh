# shellcheck shell=bash
#
#
#====\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===>
#::   \\\
#:: ======>        SHELL UTILITIES      ○
#::   ///
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>
#
# These utilities should be compatible with Bash. ZSH will load this in Bash
# emulation mode for consistency.
#
# Thanks:
#
# - https://github.com/d12frosted/environment
# - https://github.com/dylanaraps/pure-bash-bible

# shellcheck disable=2153
[[ -n ${ZSH_VERSION} ]] \
  && emulate -L bash

# Gracefully return if sourcing multiple times.
[[ -n ${UTILS_LOADED} ]] \
  && return 0
readonly UTILS_LOADED="true"

# Settings
readonly MSG__INDENT="    " # 4 spaces
readonly MSG__COL__GAP="  " # 2 spaces

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
  [[ $REPLY =~ ^[Yy]$ ]] \
    && return 0 \
    || return 1
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
# Same aspects as `msg::domain` with these differences:
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
# Same aspects as `msg::domain` with these differences:
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
  if [[ ${code} -eq 0 ]]; then
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

#====\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===>
#:
#:    ==>  SHELL  [[ shell ]]
#:
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>

# Whether the current shell is interactive.
# https://www.gnu.org/software/bash/manual/html_node/Is-this-Shell-Interactive_003f.html
function shell::is_interactive {
  [[ $- =~ 'i' ]] && return
  [[ -n ${INTERACTIVE+t} ]] && return
  [[ -n ${PS1+t} ]] && return
  return 1
}

# Whether the current shell is run within CI.
function shell::is_ci {
  [[ -n ${CI+t} ]] && return
  [[ -n ${TRAVIS+t} ]] && return
  [[ -n ${GITHUB_WORKSPACE+t} ]] && return
  return 1
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
  local forge=${2-}

  if [[ ${identifier} == "https://"* || ${identifier} == "git@"* ]]; then
    echo "${identifier}"
    return
  fi

  case $forge in
    gh | github)
      if [[ $USE_HTTPS == "true" ]]; then
        echo "https://github.com/${identifier}.git"
      else
        echo "git@github.com:${identifier}.git"
      fi
      ;;
    gl | gitlab)
      if [[ $USE_HTTPS == "true" ]]; then
        echo "https://gitlab.com/${identifier}.git"
      else
        echo "git@gitlab.com:${identifier}.git"
      fi
      ;;
    srht | sourcehut)
      if [[ $USE_HTTPS == "true" ]]; then
        echo "https://git.sr.ht/~${identifier}"
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
    gh | github) echo "gh" ;;
    gl | gitlab) echo "gl" ;;
    srht | sourcehut) echo "srht" ;;
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
