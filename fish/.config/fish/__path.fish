#!/usr/bin/env fish

# Homebrew
set -xp PATH /home/linuxbrew/.linuxbrew/sbin /home/linuxbrew/.linuxbrew/bin /usr/local/bin /usr/local/sbin

# Custom sources
set -xp PATH "$DOTS/bin" "$DOTFILES/bin" "$HOME/bin"

# PHP
set -xp PATH "$HOME/.composer/vendor/bin"

# Maid
set -xp PATH "$HOME/.maid/bin"

# Use GNU coreutils before system coreutils
set -xp PATH /usr/local/opt/coreutils/libexec/gnubin

#
# emacs / doom-emacs
#

set -xp PATH "$HOME/.emacs.d/bin"


#
# Golang
#

set -x GOPATH $HOME/gocode
set -xp PATH /usr/local/opt/go/libexec/bin
set -xp PATH $GOPATH/bin


#
# Haskell
#

set -xp PATH "$HOME/.cabal/bin"


#
# Node
#

# Add local npm bin
set -xp PATH (npm bin)
# Include the dotfiles npm bin for some "global" modules
set -xp PATH $DOTFILES/node_modules/.bin


#
# PHP
#

set -xp PATH (brew --prefix php)/bin
set -xp PATH "$HOME/.wp-cli/bin"


#
# Python
#

set -xp PATH /usr/local/opt/python/libexec/bin
set -xp PATH $HOME/.local/bin


#
# Ruby
#

set -xp PATH $HOME/.rbenv/shims
