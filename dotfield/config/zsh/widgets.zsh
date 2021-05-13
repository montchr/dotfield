# =====================================
# Expand aliases inline
# http://blog.patshead.com/2012/11/automatically-expaning-zsh-global-aliases---simplified.html
# =====================================
function globalias() {
   if [[ $LBUFFER =~ ' [A-Z0-9]+$' ]]; then
     zle _expand_alias
     zle expand-word
   fi
   zle self-insert
}
zle -N globalias


# =====================================
# Use fzf-tab after second tab press
# https://github.com/mrksr/dotfiles/blob/e3dd8bde7d7be6a294f5a2a245cb7e4a15837d71/shell/.zshrc#L101-L113
# =====================================
fzf-tab-partial-and-complete() {
  if [[ \$LASTWIDGET = 'fzf-tab-partial-and-complete' ]]; then
    fzf-tab-complete
  else
    zle complete-word
  fi
}
zle -N fzf-tab-partial-and-complete
