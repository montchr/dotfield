#!/usr/bin/env zsh

### :: HISTORY ::

##: Reference
#
# - <https://github.com/marlonrichert/zsh-launchpad/blob/84d3e3d2bc25a6590b98307e3c63b05f1fd3496a/.config/zsh/rc.d/01-hist.zsh>

# Enable additional glob operators.
# <https://zsh.sourceforge.io/Doc/Release/Expansion.html#Filename-Generation>
setopt EXTENDED_GLOB

# Glob dotfiles.
setopt GLOBDOTS

# Use modern file-locking mechanisms, for better safety & performance.
setopt HIST_FCNTL_LOCK

# Keep only the most recent copy of each duplicate entry in history.
setopt HIST_IGNORE_ALL_DUPS

# Auto-sync history between concurrent sessions.
setopt SHARE_HISTORY
