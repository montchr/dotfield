#!/usr/bin/env zsh

# fix for the big caveat when using `extendedglob` where `^` chars break command
# if the glob doesn't match anything, then just run the thing verbatim, duh come one come on
# <https://github.com/ohmyzsh/ohmyzsh/issues/449#issuecomment-6973425>
unsetopt nomatch
