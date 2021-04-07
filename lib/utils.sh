# -*- mode: sh; eval: (sh-set-shell "bash") -*-
#
# Basic Shell Utilities
#
# This file MUST remain self-contained, with no additional dependencies. It
# needs to remain lightweight enough that it can be downloaded and sourced on
# its own. The primary use case is the bootstrapping script. Without these
# utilities, the bootstrapping process would be at a disadvantage, so it
# downloads them first thing.
#


# - - - - - - - - - - - - - - - - - - - -
# Messages + Prompts
# - - - - - - - - - - - - - - - - - - - -

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



function get_current_dir () {
    local current_dir="${BASH_SOURCE%/*}"
    if [[ ! -d "${current_dir}" ]]; then current_dir="$PWD"; fi
    echo "${current_dir}"
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



# - - - - - - - - - - - - - - - - - - - -
# Repositories
# - - - - - - - - - - - - - - - - - - - -

function repo::is_repo() {
  git rev-parse &>/dev/null
}

# Qualify a repo URL.
# Parameters:
#   Repo identifier (e.g. montchr/dots) or URL.
#   Forge name or shortname (e.g. github or gh).
function repo::qualify_url() {
  local identifier=$1
  local forge=${2:-}

  if [[ "${identifier}" = "https://"* || "${identifier}" = "git@"* ]]; then
    msg::info "${identifier}"
    return
  fi

  case $forge in
    gh|github)
      if [[ "$USE_HTTPS" = "true" ]]; then
        msg::info  "https://github.com/${identifier}.git"
      else
        msg::info "git@github.com:${identifier}.git"
      fi
      ;;
    gl|gitlab)
      if [[ "$USE_HTTPS" = "true" ]]; then
        msg::info  "https://gitlab.com/${identifier}.git"
      else
        msg::info "git@gitlab.com:${identifier}.git"
      fi
      ;;
    srht|sourcehut)
      msg::error "sourcehut not yet supported!"
      return 1
      ;;
  esac
}

function repo::log() {
  git --no-pager \
      log \
      --graph \
      --pretty=format:'%Cred%h%Creset %C(bold blue)<%an> -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' \
      "$*"
}

# Get the canonical forge ID.
# Parameters:
#   Forge name
function repo::get_forge_id() {
  local forge=$1
  case $forge in
    gh|github) echo "gh" ;;
    gl|gitlab) echo "gl" ;;
    srht|sourcehut) echo "srht" ;;
  esac
}

# Synchronize a repository.
# Parameters:
#   Target directory
#   Forge name (e.g. github or gh)
#   Repo identifier (e.g. montchr/dots)
#   Branch name. Defaults to `main`.
function repo::sync() {
  local dir=$1
  local forge=$2
  local id=$3
  local branch="${4:-main}"

  local url
  local remote_branch

  msg::section "repo::sync -> $*"

  forge="$(repo::get_forge_id "${forge}")"
  remote_branch="${forge}/${branch}"

  # @TODO why is eval necessary? it's just a path
  wd=$(eval echo "${dir}")
  url=$(repo::qualify_url "${id}" "$forge")

  if [[ -d "${wd}/.git" ]]; then
    msg::info "${wd} already exists"
  else
    git clone "${url}" "${wd}" -b "${branch}"
  fi

  cd "${wd}" && {
    git diff-index --quiet HEAD -- || {
      msg::error "Your working directory is not clean."
      msg::error "Please commit or stash all changes before proceeding."
      return 1
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
