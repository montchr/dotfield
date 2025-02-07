#!/usr/bin/env zsh

### :: DIRECTORIES ::

##: Reference
#
# - <https://github.com/marlonrichert/zsh-launchpad/blob/84d3e3d2bc25a6590b98307e3c63b05f1fd3496a/.config/zsh/rc.d/02-dirs.zsh>

# Create shortcuts for frequently-accessed directories.
# Set these early, because it affects how dirs are displayed and printed.
hash -d zsh=$ZDOTDIR
# `hash -d <name>=<path>` makes ~<name> a shortcut for <path>.
# You can use this ~name anywhere you would specify a dir, not just with `cd`!

AUTO_LS_COMMANDS="eza --oneline"
AUTO_LS_NEWLINE=false
DIRSTACKSIZE=9

setopt AUTO_PUSHD           # Push the old directory onto the stack on cd.
setopt CDABLE_VARS          # Change directory to a path stored in a variable.
setopt MULTIOS              # Write to multiple descriptors.
setopt PUSHD_IGNORE_DUPS    # Don't push multiple copies of the same directory onto the directory stack.
setopt PUSHD_SILENT         # Do not print the directory stack after pushd or popd.
setopt PUSHD_TO_HOME        # Push to home directory when no argument is given.

unsetopt AUTO_NAME_DIRS     # Don't add variable-stored paths to ~ list
unsetopt CASE_GLOB          # Use case-insensitive globbing.
