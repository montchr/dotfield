#!/usr/bin/env bash

# Discover PRJ_ROOT if not found via git (i.e. not in a git repository).
# Look for the .config folder in the current directory and up
__find_prj_root_with_config_dir() (
  local old_pwd
  while [[ $old_pwd != "$PWD" ]]; do
    if [[ -d .config && "$HOME" != "$PWD" ]]; then
      echo "$PWD"
      return 0
    fi
    old_pwd=$PWD
    cd ..
  done
  # We're at the top and didn't find anything
  log_error "ERROR: could not find project root" >&2
  return 1
)

# Discover PRJ_ROOT if in a git repository.
__find_prj_root_with_git() (
  git rev-parse --show-toplevel 2>/dev/null
)

{
  : "${XDG_CONFIG_HOME:=${HOME}/.config}"
  : "${XDG_CACHE_HOME:=${HOME}/.cache}"
  : "${XDG_DATA_HOME:=${HOME}/.local/share}"

  # PRJ_ROOT
  _prj_root="$(__find_prj_root_with_git)"
  : "${PRJ_ROOT:=${_prj_root:=$(__find_prj_root_with_config_dir)}}"

  # PRJ_CONFIG_HOME - always local to the project
  : "${PRJ_CONFIG_HOME:=${PRJ_ROOT}/.config}"
  mkdir -p "${PRJ_CONFIG_HOME}"

  if [[ -z "${PRJ_ID:-}" && -f "${PRJ_CONFIG_HOME}/prj_id" ]]; then
    PRJ_ID=$(<"${PRJ_CONFIG_HOME}/prj_id")
  fi

  # PRJ_RUNTIME_DIR - always local to the project
  : "${PRJ_RUNTIME_DIR:=${PRJ_ROOT}/.run}"
  mkdir -p "${PRJ_RUNTIME_DIR}"

  # PRJ_CACHE_HOME - shared if PRJ_ID is set
  if [[ -z "${PRJ_CACHE_HOME:-}" ]]; then
    if [[ -n "${PRJ_ID:-}" ]]; then
      PRJ_CACHE_HOME="${XDG_CACHE_HOME}/prj/${PRJ_ID}"
    else
      PRJ_CACHE_HOME="${PRJ_ROOT}/.cache"
    fi
  fi
  mkdir -p "${PRJ_CACHE_HOME}"

  # PRJ_DATA_HOME - shared if PRJ_ID is set
  if [[ -z "${PRJ_DATA_HOME:-}" ]]; then
    if [[ -n "${PRJ_ID:-}" ]]; then
      PRJ_DATA_HOME="${XDG_DATA_HOME}/prj/${PRJ_ID}"
    else
      PRJ_DATA_HOME="${PRJ_ROOT}/.data"
    fi
  fi
  mkdir -p "${PRJ_DATA_HOME}"

  # PRJ_PATH - shared if PRJ_ID is set
  if [[ -z "${PRJ_PATH:-}" ]]; then
    if [[ -n "${PRJ_ID:-}" ]]; then
      PRJ_PATH="${HOME}/.local/bin/prj/${PRJ_ID}"
    else
      PRJ_PATH="${PRJ_ROOT}/.bin"
    fi
  fi
  mkdir -p "${PRJ_PATH}"
}

unset -f __find_prj_root_with_config_dir
unset -f __find_prj_root_with_git

export PATH="${PRJ_PATH}:${PATH}"

export PRJ_ROOT
export PRJ_ID
export PRJ_PATH
export PRJ_CONFIG_HOME
export PRJ_CACHE_HOME
export PRJ_DATA_HOME
export PRJ_RUNTIME_DIR
