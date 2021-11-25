# Get the current OS appearance.
#
# Returns either "light" or "dark". Defaults to "dark".
dotfield_os_appearance () {
  if [ -n "${SSH_CONNECTION}" ] && [ -n "${DOTFIELD_OS_APPEARANCE}" ]; then
    echo "%s" "${DOTFIELD_OS_APPEARANCE}"
  elif [ "$(has "dark-mode" && dark-mode status)" = "off" ]; then
    echo "light"
  else
    echo "dark"
  fi
}


# Select a color theme based on dark mode status.
#
# Accepts either on/dark or off/light. Defaults to a dark theme.
dotfield_base16_theme () {
  case $1 in
    on | dark) echo "${BASE16_THEME_DARK}" ;;
    off | light) echo "${BASE16_THEME_LIGHT}" ;;
    *) echo "${BASE16_THEME_DARK}" ;;
  esac
}


# Set the OS appearance by attempting to query the current status.
[ -z "${DOTFIELD_OS_APPEARANCE}" ] && {
  DOTFIELD_OS_APPEARANCE="$(dotfield_os_appearance)"
  export DOTFIELD_OS_APPEARANCE
}


# https://github.com/cantino/mcfly#light-mode
[ "light" = "${DOTFIELD_OS_APPEARANCE}" ] && {
  MCFLY_LIGHT=true
  export MCFLY_LIGHT
}

# Set the base16 theme based on OS appearance.
BASE16_THEME="$(dotfield_base16_theme "${DOTFIELD_OS_APPEARANCE}")"
export BASE16_THEME
