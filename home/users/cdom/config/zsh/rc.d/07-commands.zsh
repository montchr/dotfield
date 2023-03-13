#!/usr/bin/env zsh

### $DOTFIELD_USER_ZDOTDIR/rc.d/07-commands.zsh :: Commands, Functions, Hooks, Aliases...

autoload -Uz add-zsh-hook zmv

##: Create a new directory via `mkdir -p` and move into it.
function md() { [[ $# == 1 ]] && mkdir -p -- "$1" && cd -- "$1" }
compdef _directories md

##: List directory contents on arrival.
chpwd_ls() { exa --group-directories-first; }
add-zsh-hook -Uz chpwd chpwd_ls

##: Start new sessions from most recent dir
#   <https://wiki.archlinux.org/title/zsh#cdr>
# FIXME: doesn't work, but no visible errors
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs


#=====================================
#: Search for an entry in the zsh manual
#
# Usage:
#   zman <query-string>
# Parameters:
#   Search query.
#=====================================
function zman {
  PAGER="less -g -I -s '+/^       "$1"'" man zshall;
}


###: COLOR =====================================================================

#=====================================
# Print the current shell's color palette.
#
# Outputs:
#   Available colors
#=====================================
function color::palette() {
    local -a colors
    for i in {000..255}; do
        colors+=("%F{$i}$i%f")
    done
    print -cP $colors
}

#=====================================
# Print an escape sequence for a given color code.
#
# Usage:
#   color::print <color-code>
# Parameters:
#   Color code
# Outputs:
#   Escape sequence
#=====================================
function color::print() {
    local color="%F{$1}"
    echo -E ${(qqqq)${(%)color}}
}
