# -------------------------------------
#  FZF-TAB COMPLETIONS
# -------------------------------------
# https://github.com/Aloxaf/fzf-tab#configure


# disable sort when completing `git checkout`
zstyle ':completion:*:git-checkout:*' sort false

# preview directory's content with exa when completing cd
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always --icons --all $realpath'

# Display completion groups as headers
zstyle ':fzf-tab:*' show-group full

# TODO: choose better keys -- switching group with `.` means we can't find hidden files
# zstyle ':fzf-tab:*' switch-group ',' '.'

# Prefix to indicate color
zstyle ':fzf-tab:*' prefix '· '

zstyle ':fzf-tab:*' fzf-bindings 'tab:accept'


# Use fzf-tab after second tab press
# via https://github.com/mrksr/dotfiles/blob/e3dd8bde7d7be6a294f5a2a245cb7e4a15837d71/shell/.zshrc#L101-L113
fzf-tab-partial-and-complete() {
  if [[ $LASTWIDGET == 'fzf-tab-partial-and-complete' ]]; then
    fzf-tab-complete
  else
    zle complete-word
  fi
}
zle -N fzf-tab-partial-and-complete
bindkey '^I' fzf-tab-partial-and-complete
