#!/usr/bin/env zsh

# fix for the big caveat when using `extendedglob` where `^` chars break command
# if the glob doesn't match anything, then just run the thing verbatim, duh come one come on
# <https://github.com/ohmyzsh/ohmyzsh/issues/449#issuecomment-6973425>
unsetopt nomatch

function chpwd-list-all() {
    emulate -L zsh
    eza --all --group-directories-first --grid
}
add-zsh-hook -Uz chpwd chpwd-list-all

# Support for Emacs EAT package
# <https://codeberg.org/akib/emacs-eat>
[[ -n "$EAT_SHELL_INTEGRATION_DIR" ]] && \
  source "$EAT_SHELL_INTEGRATION_DIR"
