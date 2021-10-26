#!/usr/bin/env zsh
# https://github.com/hlissner/dotfiles/blob/master/config/emacs/aliases.zsh

e()     { pgrep emacs && emacsclient -n "$@" || emacs -nw "$@" }
ediff() { emacs -nw --eval "(ediff-files \"$1\" \"$2\")"; }
eman()  { emacs -nw --eval "(switch-to-buffer (man \"$1\"))"; }
ekill() { emacsclient --eval '(kill-emacs)'; }
