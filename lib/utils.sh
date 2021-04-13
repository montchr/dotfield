# -*- mode: sh; eval: (sh-set-shell "bash") -*-
# shellcheck shell=bash
#
#
#====\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===>
#::   \\\
#:: ======>     THE WORLD   ○    [[  shell utilities  ]]
#::   ///
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>
#
#
# This file MUST remain self-contained, with no additional dependencies. The
# primary use case is the bootstrapping script. Without these utilities, the
# bootstrapping process would be at a disadvantage, so it downloads them first
# thing.
#
# Thanks:
# - https://github.com/dylanaraps/pure-bash-bible
#

set +u
# shellcheck disable=2153
[[ -n "$ZSH_VERSION" ]] \
  && emulate -L bash
set -u

USER="${USER:-}"
if [[ -z "${USER}" ]]; then
  USER="$(whoami)"
fi

KERNEL_NAME="${KERNEL_NAME:-}"
if [[ -z "${KERNEL_NAME}" ]]; then
  KERNEL_NAME="$( uname -s | tr '[:upper:]' '[:lower:]')"
fi

OS_NAME="${OS_NAME:-}"
OS_VERSION="${OS_VERSION:-}"
if [[ -z "${OS_NAME}" || -z "${OS_VERSION}" ]]; then
  case "${KERNEL_NAME}" in
    darwin)
      OS_NAME="macos"
      OS_VERSION="$(sw_vers -productVersion)"
      ;;
    linux)
      # shellcheck disable=1091
      OS_NAME="$(
        . /etc/os-release
        printf "%s" "${ID}"
      )"
      # shellcheck disable=1091
      OS_VERSION="$(
        . /etc/os-release
        printf "%s" "${VERSION_ID}"
      )"
      ;;
    *) OS_NAME="unknown" ;;
  esac
fi

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

export \
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


#========================================
# Print basic information about the known world.
#
# Usage:
#   world.info
# Globals:
#   KERNEL_NAME
#   KERNEL_RELEASE
#   OS_NAME
#   OS_VERSION
#   USER
#   XDG_BIN_HOME
#   XDG_CONFIG_HOME
# Outputs:
#   Values of various key global variables.
#========================================
function world.info() {

  echo "Kernel name:      $KERNEL_NAME"
  echo "Kernel release:   $KERNEL_RELEASE"
  echo "Operating system: $OS_NAME"
  echo "OS version:       $OS_VERSION"
  echo "User:             $USER"
  echo "XDG_CONFIG_HOME:  $XDG_CONFIG_HOME"
  echo "XDG_BIN_HOME:     $XDG_BIN_HOME"
  echo

}


#====\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===>
#::
#::  ==>  STRING MANIPULATION  <==  :::
#::
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>


#========================================
# Convert a string to lowercase.
# 
# Usage:
#   string.lower <string|@string>
# Parameters:
#   String. Optionally prefixed by `@` for a nameref.
# Outputs:
#   Modified string, unless input is prefixed with `@`.
#========================================
function string.lower {
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
#   string.upper <string|@string>
# Parameters:
#   String. Optionally prefixed by `@` for a nameref.
# Outputs:
#   Modified string, unless input is prefixed with `@`.
#========================================
function string.upper {
  if [[ "@" == "$1"* ]]; then
    local -n str=${1}
    str="${str^^}"
    return
  else
    printf '%s\n' "${1^^}"
  fi
}

# Sanitize a string, leaving only alphanumeric characters, periods, dashes, and
# underscores.
#
# Parameters:
#   String...
function string.sanitize {
  local clean="$*"
  clean="${clean//[[:space:]]/\-}"
  clean="${clean//"/"/_}"
  clean="${clean//[^[:word:].-]}"
  string.lower "${clean}"
}

function string.is_supported_version {
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
#::
#::  ==>  MESSAGES + PROMPTS  <==  :::
#::
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>


# Prompt the user for input.
# Parameters:
#   Prompt message
function msg.ask {
  msg.question "$1"
  read -r
}

# Prompt the user for input without echoing response.
# Parameters:
#   Prompt message
function msg.ask_silently {
  msg.question "$1"
  read -s -r
  printf "\n"
}

function msg.ask_for_confirmation {
  msg.question "$1 (y/n) "
  read -r -n 1
  printf "\n"
}

# Get the user's answer to the previous prompt.
function msg.get_answer {
  printf "%s" "$REPLY"
}

# Whether the user responded affirmatively to the previous prompt.
# Globals:
#   REPLY
function msg.is_confirmed {
  [[ "$REPLY" =~ ^[Yy]$ ]] &&
    return 0 ||
      return 1
}


# Print a top-level heading message.
# Deprecated.
# Parameters:
#   Message
function msg.hed {
  msg.section "$@"
}

# Print a subheading message.
# Parameters:
#   Section name
#   Message
function msg.subhed {
  msg.subsection "$@"
}

function msg.section {
  msg.in_green "\n => $1\n"
}

function msg.domain {
  msg.in_green "\n => $1 :: ${*:2}\n"
}

function msg.domain__lesser {
  msg.in_purple "\n -> $1 :: ${*:2}\n"
}

function msg.domain__inactive {
  print "\n <- $1 :: ${*:2}\n"
}

# Print a basic informational message.
# Parameters:
#   Message
function msg.info {
  print "\n${MSG_INDENT}${1}\n"
}

# Prompt the user for a response to a question.
# Parameters:
#   Message
function msg.question {
  msg.in_yellow "${MSG_INDENT}[?] $1"
}

# Print a message along with an indication of the result of the previous command.
# Parameters:
#   Result code
#   Message
function msg.result {
  if [ "$1" -eq 0 ]; then
    msg.success "$2"
  else
    msg.error "$2"
  fi
  return "$1"
}

# Print a message indicating success.
# Parameters:
#   Message
function msg.success {
  msg.in_green "${MSG_INDENT}[✔] $1\n"
}

# Print a message indicating a warning.
# Parameters:
#   Message
function msg.warning() {
  msg.in_yellow "${MSG_INDENT}[!] $1\n"
}

# Print a message indicating an error.
# Parameters:
#   Message
function msg.error {
  msg.in_red "${MSG_INDENT}[✖] $*\n"
}

function msg.stream_errors {
  while read -r line; do
    msg.error "↳ ERROR: $line"
  done
}

function msg.in_green {
  msg.in_color "$1" 2
}

function msg.in_purple {
  msg.in_color "$1" 5
}

function msg.in_red {
  msg.in_color "$1" 1
}

function msg.in_yellow {
  msg.in_color "$1" 3
}

function msg.in_color {
  printf "%b" \
    "$(tput setaf "$2" 2>/dev/null)" \
    "$1" \
    "$(tput sgr0 2>/dev/null)"
}

function msg.spinner {
  local -r PID="$1"
  local -r CMDS="$2"
  local -r MSG="$3"

  local -r FRAMES='/-\|'
  local -r NUMBER_OR_FRAMES=${#FRAMES}
  local i=0
  local frameText=""

  if ! shell.is_ci; then
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

    if shell.is_ci; then
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
#::
#::  ==>  SHELL  <==  :::
#::
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>


function shell.kill_all_subprocesses {
  local i=""
  for i in $(jobs -p); do
    kill "$i"
    wait "$i" &>/dev/null
  done
}

function shell.execute {
  local -r CMDS="$1"
  local -r MSG="${2:-$1}"
  local -r TMP_FILE="$(mktemp /tmp/XXXXX)"
  local exitCode=0
  local cmdsPID=""

  # If the current process is ended, also end all its subprocesses.
  shell.set_trap "EXIT" "kill_all_subprocesses"

  # Execute commands in background
  eval "$CMDS" \
    2>"$TMP_FILE" &

  cmdsPID=$!

  # Show a spinner if the commands require more time to complete.
  msg.spinner "$cmdsPID" "$CMDS" "$MSG"

  # Wait for the commands to no longer be executing in the background, and then
  # get their exit code.
  wait "$cmdsPID" &>/dev/null
  exitCode=$?

  # Print output based on what happened.
  msg.result $exitCode "$MSG"

  if [[ $exitCode -ne 0 ]]; then
    msg.error_stream <"$TMP_FILE"
  fi

  rm -rf "$TMP_FILE"

  return $exitCode
}

function shell.ask_for_sudo {
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
function shell.is_interactive {
  [[ $- =~ 'i' ]] && return
  local vars=(INTERACTIVE PS1)
  for var in "${vars[@]}"; do
    [[ -v "${var}" ]] && return
  done
  return 1
}

# Whether the current shell is run within CI or Vagrant.
function shell.is_ci {
  local vars=(CI TRAVIS VAGRANT)
  for var in "${vars[@]}"; do
    [[ -v "${var}" ]] && return
  done
  return 1
}

function shell.set_trap {
  trap -p "$1" | grep "$2" &>/dev/null ||
    trap '$2' "$1"
}

function shell.has {
  command -v "$1" >/dev/null 2>&1
}


#====\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===>
#::
#::  ==>  FILESYSTEM  <==  :::
#::
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>


#======================================
# Get the basename of a file path.
#
# Alternative to the external `basename`.
#
# https://github.com/dylanaraps/pure-bash-bible#get-the-base-name-of-a-file-path
#
# Usage:
#   fs.basename <path> [suffix]
#
# Arguments:
#   File path.
#   File suffix. Optional.
# Outputs:
#   STDOUT - Basename. Includes file extension unless specifying [suffix].
#========================================
function fs.basename {
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
#   fs.dirname <path>
# Arguments:
#   File path.
# Outputs:
#   STDOUT - Directory name.
# Returns:
#   0 - If at root directory or no directory.
#========================================
function fs.dirname {
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


function fs.ensure_dir {
  if [[ ! -d "$1" ]]; then
    msg.info "create $1"
    mkdir -p "$1"
  fi
  msg.error $? "$1"
}


function fs.linkfile {
  local file="$1"
  [[ -f "${file}" ]] && (
    cd "$(fs.dirname "${file}")" \
      || return 1
    fs.map_lines \
      fs.link \
      "${file}"
  )
}

# @TODO untested
function fs.link_all {
  local src_dir="$1"
  local target_dir="$2"

  if ! [[ -d "${src_dir}" ]]; then
    msg.error "[Error] Source '${src_dir}' is not a directory! Aborting."
  fi
  if ! [[ -d "${target_dir}" ]]; then
    msg.error "[Error] Target '${target_dir}' is not a directory! Aborting."
  fi

  msg.warning "Linking all files in '${src_dir}' to '${target_dir}':"
  for f in "${src_dir}"/*; do
    msg.info "$f"
  done

  for f in "${src_dir}"/*; do
    fs.link "$f" "${target_dir}/$(basename "$f")"
  done
}


#========================================
# Link files or directories safely.
#
# Usage:
#   fs.link <src> <dest>
# Parameters:
#   Source path (relative)
#   Target path (absolute)
#========================================
function fs.link {
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
    owner=$(stat -c '%U' "${target_dir}")
    if [[ "${owner}" != "root" && "${owner}" != "${USER}" ]]; then
      msg.error "can not link '${src_abs_path}' to '${target_path}'"
      msg.error "owner of '${target_dir}' is ${owner}"
      msg.error "allowed owners: root or ${USER}"
      exit 1
    fi
  fi

  if [[ ! -f "${src_abs_path}" && ! -d "${src_abs_path}" ]]; then
    msg.error "can not link '${src_abs_path}' as it does not exist"
    exit 1
  fi

  if [[ ! -d "${target_dir}" ]]; then
    msg.info "create ${target_dir}"
    mkdir -p "${target_dir}"
  fi

  if [[ -L "${target_path}" ]]; then
    msg.info "relink ${src_abs_path} -> ${target_path}"
    if [[ "${owner}" = "root" ]]; then
      sudo rm "${target_path}"
    else
      rm "${target_path}"
    fi
  else
    msg.info "link ${src_abs_path} -> ${target_path}"
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
#   fs.combine <file>...
# Parameters:
#   Files...
# Outputs:
#   Path to the combined file.
#========================================
function fs.combine {
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
#   fs.map_lines <callback> <file>
# Parameters:
#   Command to run for each line in a file
#   File containing argument lists separated by linebreaks.
#========================================
function fs.map_lines {
  local callback="$1"
  local file="$2"

  [[ ! -f "${file}" ]] \
    && return 1

  while IFS='' read -r line || [[ -n "${line}" ]]; do
    # Ignore comment lines
    [[ "${line}" == "#"* ]] && continue
    # Pass the entire line as-is -- words will be split for separate args.
    # shellcheck disable=2086
    ${callback} ${line}
  done < "${file}"
}


#========================================
# Download a remote file, linking it to the current user's bin directory.
#
# Usage:
#   fs.download_bin <name> <URL>
# Parameters:
#   Executable name
#   Source URL
#========================================
function fs.download_bin {
  local target="${XDG_BIN_HOME:-${${HOME}/.local/bin}}/${1}"
  local src="$2"
  curl --silent -o "${target}" "${src}"
  chmod a+x "${target}"
  hash -r
}


#====\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===>
#::
#::  ==>  REPOSITORIES  <==  :::
#::
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>


function repo.is_repo {
  git rev-parse &>/dev/null
}

# Qualify a repo URL.
# Parameters:
#   Repo identifier (e.g. montchr/dots) or URL.
#   Forge name or shortname (e.g. github or gh).
function repo.qualify_url {
  local identifier=$1
  local forge=${2:-}

  if [[ "${identifier}" = "https://"* || "${identifier}" = "git@"* ]]; then
    msg.info "${identifier}"
    return
  fi

  case $forge in
    gh|github)
      if [[ "$USE_HTTPS" = "true" ]]; then
        msg.info  "https://github.com/${identifier}.git"
      else
        msg.info "git@github.com:${identifier}.git"
      fi
      ;;
    gl|gitlab)
      if [[ "$USE_HTTPS" = "true" ]]; then
        msg.info  "https://gitlab.com/${identifier}.git"
      else
        msg.info "git@gitlab.com:${identifier}.git"
      fi
      ;;
    srht|sourcehut)
      msg.error "sourcehut not yet supported!"
      return 1
      ;;
  esac
}

function repo.log {
  git --no-pager \
      log \
      --graph \
      --pretty=format:'%Cred%h%Creset %C(bold blue)<%an> -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' \
      "$*"
}

# Get the canonical forge ID.
# Parameters:
#   Forge name
function repo.get_forge_id {
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
function repo.sync {
  local dir=$1
  local forge=$2
  local id=$3
  local branch="${4:-main}"

  local url
  local remote_branch

  msg.section "repo.sync -> $*"

  forge="$(repo.get_forge_id "${forge}")"
  remote_branch="${forge}/${branch}"

  # @TODO avoid eval
  wd=$(eval echo "${dir}")
  url=$(repo.qualify_url "${id}" "$forge")

  if [[ -d "${wd}/.git" ]]; then
    msg.info "${wd} already exists"
  else
    git clone "${url}" "${wd}" -b "${branch}"
  fi

  cd "${wd}" && {
    git diff-index --quiet HEAD -- || {
      msg.error "Your working directory is not clean."
      msg.error "Please commit or stash all changes before proceeding."
      return 1
    }

    current_branch=$(git symbolic-ref --short HEAD)
    if [[ "${branch}" != "${current_branch}" ]]; then
      msg.info "Switching from ${current_branch} to ${branch}"
      git checkout "${branch}"
    fi

    # Note that the remote will be named after the forge, not 'origin'.
    if [[ -d ".git/refs/remotes/${forge}" ]]; then
      current_url=$(git remote get-url "${forge}")
      if [[ "${current_url}" != "${url}" ]]; then
        msg.warning "Remote '${forge}' has wrong url, so updating it"
        msg.warning "  ${current_url} -> ${url}"
        git remote set-url "${forge}" "$url"
      fi
    else
      msg.warning "Could not find remote '${forge}', so adding it"
      git remote add "${forge}" "${url}"
    fi

    msg.info "fetch ${forge}"
    git fetch "${forge}"
    if [[ $(git rev-parse HEAD) == $(git rev-parse "${remote_branch}") ]]; then
      msg.success "Everything up-to-date"
      return 0
    fi

    if [ "$(git rev-list "HEAD..${remote_branch}" --count)" != 0 ]; then
      msg.info "Fetched changes:"
      repo.log "HEAD..${remote_branch}"
      msg.info
    fi

    msg.info "rebase onto ${remote_branch}"
    git rebase "${remote_branch}"

    if [[ "${url}" = *"${QUERENT}"* ]]; then
      if [ "$(git rev-list "${remote_branch}..HEAD" --count)" != 0 ]; then
        msg.info "Changes to push:"
        repo.log "${remote_branch}..HEAD"
        msg.info
      fi

      msg.info "pushing changes"
      git push "${forge}" "${branch}"
    fi
  }
}


#====\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===>
#::
#::  ==>  GUARDIANS  <==  :::
#::
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>


function guard.domain {
  local domain=$1
  local key
  key=$(string.lower "${domain}")
  shift

  local message="$*"

  local guard_ref="guard_$key"
  local ignore_guard_ref="guard_ignore_$key"
  guard="${!guard_ref}"
  ignore_guard="${!ignore_guard_ref}"

  if [[ ("$ALL" == "true" || "$guard" == "true") && "$ignore_guard" == "" ]]; then
    msg.domain__lesser "${domain}" "${message}"
    return 0
  else
    msg.domain__inactive "${domain}" "${message}"
    return 1
  fi
}

function guard.install {
  [[ "$ACTION" == "install" ]]
  return
}

function guard.upgrade {
  [[ "$ACTION" == "upgrade" ]]
  return
}

function guard.test {
  [[ "$ACTION" == "test" ]]
  return
}

function guard.ubuntu {
  [[ "$OS_NAME" == "ubuntu" ]]
  return
}

function guard.arch {
  [[ "$OS_NAME" == "arch" ]]
  return
}

function guard.macos {
  [[ "$OS_NAME" == "macos" ]]
  return
}

function guard.root {
  [[ "$(whoami)" == "root" ]]
  return
}


#====\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===\\===\\\===>
#:
#:   ==>  USER MANAGEMENT  <==  :::
#:
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>


# Execute a command as a certain user
# Arguments:
#   Account Username
#   Command to be executed
function user.exec () {
  local username=${1}
  local exec_command=${2}
  sudo -u "${username}" -H bash -c "${exec_command}"
}

# Create the new user account.
# Arguments:
#   Account Username
#   Account Password
function user.create_account () {
  local username=${1}
  local password=${2}
  sudo adduser --disabled-password --gecos '' "${username}"
  echo "${username}:${password}" | sudo chpasswd
  sudo usermod -aG sudo "${username}"
}


# Set a global $PASSWORD variable by generation or by prompt.
#
# Globals:
#   PASSWORD
function user.set_password_global {
  msg.ask_for_confirmation "Do you want to generate a new password?"
  if msg.is_confirmed; then
    if shell.has bw; then
      PASSWORD=$(bw generate --words 3 --separator '.' -p)
      msg.success "Generated new password:"
      msg.info    "    ${PASSWORD}" ; printf "\n"
    else
      msg.error "[Error]" "Couldn't find a password generator!"
    fi
  else
    user.prompt_for_password
  fi
}

# Keep prompting for the password and password confirmation.
#
# Globals:
#   PASSWORD
function user.prompt_for_password {
  local passwords_match=0
  local confirmation
  while [[ "${passwords_match}" -eq "0" ]]; do
    msg.ask_silently "Enter new password:"
    PASSWORD=$(msg.get_answer)

    msg.ask_silently "Confirm password:"
    confirmation=$(msg.get_answer)

    if [[ "${PASSWORD}" != "${confirmation}" ]]; then
      msg.error "Passwords do not match! Please try again."
    else
      passwords_match=1
    fi
  done
}


# Allow passwordless sudo for a user.
# Parameters:
#   Username
function user.allow_passwordless_sudo () {
  local username="${1}"
  sudo cp /etc/sudoers /etc/sudoers.bak
  sudo bash -c "echo '${1} ALL=(ALL) NOPASSWD: ALL' | (EDITOR='tee -a' visudo)"
}

# Add an SSH public key for a user.
# Parameters:
#   Username
#   SSH public key
function user.add_ssh_pub_key () {
  local username=${1}
  local pubkey=${2}

  user.exec "${username}" \
    "mkdir -p ~/.ssh; chmod 700 ~/.ssh; touch ~/.ssh/authorized_keys"
  user.exec "${username}" \
    "echo \"${pubkey}\" | sudo tee -a ~/.ssh/authorized_keys"
  user.exec "${username}" \
    "chmod 600 ~/.ssh/authorized_keys"
}

# Modify the sshd_config file.
function user.change_ssh_config () {
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
#:   ==>  PACKAGE MANAGEMENT  <==   [[ pkg ]]
#:
#====///===//===///===//===///===//===///===//===///===//===///===//===///===>


#
# : Debian/Ubuntu :: [apt]
# - - - - - - - - - - - - - - - - - - -

# Resynchronize the package index files from their sources.
function pkg.apt.update_repos () {
  shell.execute \
    "sudo apt-get update -qqy" \
    "APT (update)"
}

# Install the newest versions of all installed packages.
function pkg.apt.upgrade_all() {
  shell.execute \
    "export DEBIAN_FRONTEND=\"noninteractive\" \
      && sudo apt-get -o Dpkg::Options::=\"--force-confnew\" upgrade -qqy" \
    "APT (upgrade)"
}

# Install a package.
# Parameters:
#   Package description
#   Package name
#   Extra arguments to apt-get
function pkg.apt.install () {
  local DESCRIPTION="$1"
  local PACKAGE="$2"
  local EXTRA_ARGUMENTS="$3"

  if ! pkg.apt.exists "${PACKAGE}"; then
    shell.execute \
      "sudo apt-get install --allow-unauthenticated -qqy ${EXTRA_ARGUMENTS} ${PACKAGE}" \
      "${DESCRIPTION}"
  else
    msg.success "${DESCRIPTION}"
  fi
}

# Whether a package is installed.
# Parameters:
#   Package name
function pkg.apt.exists () {
  dpkg -s "$1" &> /dev/null
}
