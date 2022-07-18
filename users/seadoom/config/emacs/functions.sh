# https://github.com/hlissner/dotfiles/blob/master/config/emacs/aliases.zsh

e() {
  if [ "$(pgrep emacs)" ]; then
    emacsclient -n "$@"
  else
    emacs -nw "$@"
  fi
}

ediff() {
  emacs -nw --eval "(ediff-files \"$1\" \"$2\")";
}

eman() {
  emacs -nw --eval "(switch-to-buffer (man \"$1\"))";
}

ekill() {
  emacsclient --eval '(kill-emacs)';
}
