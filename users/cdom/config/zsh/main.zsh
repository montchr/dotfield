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

# Support for OSC-7 escape sequence, spawning new terminal instances in the
# current working directory.
#
# <https://codeberg.org/dnkl/foot/wiki#spawning-new-terminal-instances-in-the-current-working-directory>

function osc7-pwd() {
    emulate -L zsh # also sets localoptions for us
    setopt extendedglob
    local LC_ALL=C
    printf '\e]7;file://%s%s\e\' $HOST ${PWD//(#m)([^@-Za-z&-;_~])/%${(l:2::0:)$(([##16]#MATCH))}}
}

function chpwd-osc7-pwd() {
    (( ZSH_SUBSHELL )) || osc7-pwd
}
add-zsh-hook -Uz chpwd chpwd-osc7-pwd
