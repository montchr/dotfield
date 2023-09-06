#!/usr/bin/env zsh

### :: COMMANDS :: and functions, hooks, aliases...

# TODO: explain this!
autoload -Uz add-zsh-hook zmv

##: Create a new directory via `mkdir -p` and move into it.
function md() { [[ $# == 1 ]] && mkdir -p -- "$1" && cd -- "$1" }
compdef _directories md

##: List directory contents on arrival.
chpwd_ls() { eza --group-directories-first; }
add-zsh-hook -Uz chpwd chpwd_ls

##: Start new sessions from most recent dir (<https://wiki.archlinux.org/title/zsh#cdr>)
# NOTE: doesn't work in kitty, maybe others?
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs


#=====================================
#: Grep raw zsh config state for pattern.
#
# Example usage: finding the source of an unknown alias.
#
# via <https://superuser.com/a/1097586>
#
# Usage:
#   zcfgrep <pattern> [<args>...]
# Parameters:
#   ripgrep args
# Outputs:
#   ripgrep output
#=====================================
function zcfgrep() {
  local expr="$1"
  shift
  PS4='+%x:%I>' zsh -i -x -c '' |& rg "$expr" "$@"
}


#=====================================
#: Search for an entry in the zsh manual
#
# Usage:
#   zman <query-string>
# Parameters:
#   Search query.
#=====================================
function zman() {
  PAGER="less -g -I -s '+/^       "$1"'" \
    man zshall
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
