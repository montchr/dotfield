#!/usr/bin/env bash

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
