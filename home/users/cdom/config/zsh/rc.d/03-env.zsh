#!/usr/bin/env zsh

### $DOTFIELD_USER_ZDOTDIR/rc.d/03-env.zsh :: Environment Variables

##: Reference
#
# - <https://github.com/marlonrichert/zsh-launchpad/blob/84d3e3d2bc25a6590b98307e3c63b05f1fd3496a/.config/zsh/rc.d/04-env.zsh>

# `-U` => Discards duplicates
# `-T` => Creates a "tied" pair; see below.
export -U PATH path FPATH fpath MANPATH manpath
export -UT INFOPATH infopath

# $PATH and $path (and also $FPATH and $fpath, etc.) are "tied" to each other.
# Modifying one will also modify the other.
# Note that each value in an array is expanded separately. Thus, we can use ~
# for $HOME in each $path entry.
# TODO: configure appropriately
# path=(
#     /home/linuxbrew/.linuxbrew/bin(N)   # (N): null if file doesn't exist
#     $path
#     ~/.local/bin
# )

# Add your functions to $fpath for autoloading
fpath=(
    $ZDOTDIR/functions
    $fpath
)
