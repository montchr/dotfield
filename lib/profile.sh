# -*- mode: sh; eval: (sh-set-shell "sh") -*-
#
# shellcheck shell=sh
#
# ~/.profile
#
# (Nearly)-POSIX-compliant core shell environment setup file.
#
# Ideally, this should be sourced by all shells as early as possible, as it sets
# important environment variables.
#
# Bash:  source within ~/.bashrc
# ZSH:   source within ~/.zshenv (reloads on every prompt, so keep this file light!)
#


# - - - - - - - - - - - - - - - - - - - -
# Helpers
# - - - - - - - - - - - - - - - - - - - -

# Check whether a command exists.
has() {
  type "$1" >/dev/null 2>&1
}

# Detect the OS and export appropriate variables.
# @TODO fails shellcheck for sh!
case "${OSTYPE}" in
  linux*)
    IS_LINUX=true
    KERNEL_NAME="linux"
    ;;
  darwin*)
    IS_MAC=true
    KERNEL_NAME="darwin"
    ;;
esac

export \
  IS_LINUX \
  IS_MAC \
  KERNEL_NAME


# - - - - - - - - - - - - - - - - - - - -
# Core Configuration
# - - - - - - - - - - - - - - - - - - - -

# Define XDG base directories before all others.
# https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
: "${XDG_CACHE_HOME:=${HOME}/.cache}"
: "${XDG_CONFIG_HOME:=${HOME}/.config}"
: "${XDG_DATA_HOME:=${HOME}/.local/share}"
: "${XDG_RUNTIME_DIR:=/tmp}"

# Non-standard XDG-inspired locations
: "${XDG_BIN_HOME:=${HOME}/.local/bin}"
: "${XDG_LIB_HOME:=${HOME}/.local/lib}"

: "${TMPDIR:=${XDG_RUNTIME_DIR}}"
CACHEDIR="${XDG_CACHE_HOME}"

export \
  XDG_CACHE_HOME \
  XDG_CONFIG_HOME \
  XDG_DATA_HOME \
  XDG_RUNTIME_DIR \
  XDG_BIN_HOME \
  XDG_LIB_HOME \
  CACHEDIR \
  TMPDIR


# - - - - - - - - - - - - - - - - - - - -
# Local Configuration + Identity
# - - - - - - - - - - - - - - - - - - - -

# This should indeed point to XDG_CONFIG_HOME, not the DOTFIELD_DIR, as it is
# host-local.
DOTFIELD_LOCAL_CONFIG="${XDG_CONFIG_HOME}/shell/profile.local"
[ -z "${DOTFIELD_LOCAL_LOADED}" ] && [ -f "${DOTFIELD_LOCAL_CONFIG}" ] && {
  # shellcheck disable=SC1090
  . "${DOTFIELD_LOCAL_CONFIG}"
}


# - - - - - - - - - - - - - - - - - - - -
# App Configuration
# - - - - - - - - - - - - - - - - - - - -
#
# Sources:
#   - https://wiki.archlinux.org/title/XDG_Base_Directory
#

# Composer
export COMPOSER_HOME="${XDG_CONFIG_HOME}/composer"

# Docker
export \
  DOCKER_CONFIG="${XDG_CONFIG_HOME}/docker" \
  MACHINE_STORAGE_PATH="${XDG_DATA_HOME}/docker-machine"

# Go
export GOPATH="${XDG_DATA_HOME}/go"

# Kitty terminal
export \
  KITTY_CONFIG_DIRECTORY="${XDG_CONFIG_HOME}/kitty" \
  KITTY_SOCKET="unix:${XDG_RUNTIME_DIR}/kitty-socket"

# Node + npm + nvm
export \
  NODE_REPL_HISTORY="${XDG_DATA_HOME}/node/repl_history" \
  NVM_DIR="${XDG_DATA_HOME}/node/nvm" \
  NVM_AUTO_USE=true \
  NVM_BIN="${XDG_BIN_HOME}" \
  NVM_COMPLETION=true \
  NVM_LAZY_LOAD=true

# TODO: remove? commented out to avoid issues with npm PATH
# export NPM_CONFIG_USERCONFIG="${XDG_CONFIG_HOME}/npm/npmrc"

# Pass
export PASSWORD_STORE_DIR="${XDG_DATA_HOME}/pass"

# Promnesia
export PROMNESIA_CONFIG="${XDG_CONFIG_HOME}/promnesia/config.py"

# Readline
export INPUTRC="${XDG_CONFIG_HOME}/readline/inputrc"

# Ruby / Bundler
export \
  BUNDLE_USER_CACHE="${XDG_CACHE_HOME}/bundle" \
  BUNDLE_USER_CONFIG="${XDG_CONFIG_HOME}/bundle" \
  BUNDLE_USER_PLUGIN="${XDG_DATA_HOME}/bundle"

# Rust
export \
  CARGO_HOME="${XDG_DATA_HOME}/cargo" \
  RUSTUP_HOME="$XDG_DATA_HOME/rustup"

# GNU Screen
export SCREENRC="${XDG_CONFIG_HOME}/screen/screenrc"

# Vagrant
export \
  VAGRANT_ALIAS_FILE="${XDG_DATA_HOME}/vagrant/aliases" \
  VAGRANT_HOME="${XDG_DATA_HOME}/vagrant"

# Vim
# Coax Vim into sourcing its config from XDG config home.
# TODO: why is this disabled?
# shellcheck disable=2016
# export VIMINIT='source "$XDG_CONFIG_HOME/vim/vimrc"'

# wd
# https://github.com/mfaerevaag/wd
export WD_CONFIG="${XDG_CONFIG_HOME}/wd/warprc"


# - - - - - - - - - - - - - - - - - - - -
# PATH Modifications
# - - - - - - - - - - - - - - - - - - - -

export PATH="${XDG_BIN_HOME}:${PATH}"

# Required by Homebrew
# export PATH="/usr/local/sbin:$PATH"
# export PATH="/usr/local/bin:$PATH"
# export PATH="/home/linuxbrew/.linuxbrew/bin:$PATH"

# Prefer GNU coreutils
# export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
# export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
# Prefer brew's sqlite3
# export PATH="/usr/local/opt/sqlite/bin:$PATH"

# Doom Emacs
export PATH="${EMACSDIR}/bin:$PATH"

# Ensure Composer bin is low priority
export PATH="$PATH:${COMPOSER_HOME}/vendor/bin"


# - - - - - - - - - - - - - - - - - - - -
# - Personal Preferences
# - System Appearance
# - - - - - - - - - - - - - - - - - - - -

# System editor settings.
# TODO: this is also set in nix emacs module -- pick one or the other
export \
  EDITOR="emacsclient" \
  GIT_EDITOR="$EDITOR" \
  SYSTEMD_EDITOR="$EDITOR"

export BASE16_THEME_DARK='black-metal-khold' \
  BASE16_THEME_LIGHT='grayscale-light'

# Emacs-specific configuration.
export \
  CDOM_EMACS_THEME_DARK="modus-vivendi" \
  CDOM_EMACS_THEME_LIGHT="modus-operandi"

# Get the current OS appearance.
#
# Returns either "light" or "dark". Defaults to "dark".
cdom_os_appearance () {
  if [ -n "${SSH_CONNECTION}" ] && [ -n "${CDOM_OS_APPEARANCE}" ]; then
    echo "%s" "${CDOM_OS_APPEARANCE}"
  elif [ "$(has "dark-mode" && dark-mode status)" = "off" ]; then
    echo "light"
  else
    echo "dark"
  fi
}

# Select a color theme based on dark mode status.
#
# Accepts either on/dark or off/light. Defaults to a dark theme.
cdom_base16_theme () {
  case $1 in
    on | dark) echo ${BASE16_THEME_DARK} ;;
    off | light) echo ${BASE16_THEME_LIGHT} ;;
    *) echo ${BASE16_THEME_DARK} ;;
  esac
}

# Set the OS appearance by attempting to query the current status.
[ -z "${CDOM_OS_APPEARANCE}" ] && {
  CDOM_OS_APPEARANCE="$(cdom_os_appearance)"
  export CDOM_OS_APPEARANCE
}

# Set the base16 theme based on OS appearance.
BASE16_THEME="$(cdom_base16_theme "${CDOM_OS_APPEARANCE}")"
export BASE16_THEME
