# Complete from both ends of a word.
setopt complete_in_word
# Move cursor to the end of a completed word.
setopt always_to_end
# Perform path search even on command names with slashes.
setopt path_dirs
# Show completion menu on a successive tab press.
setopt auto_menu
# Automatically list choices on ambiguous completion.
setopt auto_list
# Do not autoselect the first completion entry.
unsetopt menu_complete
# If completed parameter is a directory, add a trailing slash.
setopt auto_param_slash
# Disable start/stop characters in shell editor.
# unsetopt flow_control



# zstyle ':completion:*' verbose yes
zstyle ':completion:*' group-name ''
zstyle ':completion:*' rehash true

# Fuzzy matching for typos
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3>7?7:($#PREFIX+$#SUFFIX)/3))numeric)'

# set list-colors to enable filename colorizing
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

zstyle ':completion:*:default' list-prompt '%S%M matches%s'

# set descriptions format to enable group support
zstyle ':completion:*:descriptions' format '[%d]'

zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec))'

# disable sort when completing `git checkout`
zstyle ':completion:*:git-checkout:*' sort false

zstyle ':completion:*:matches' group 'yes'

zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'

# fzf-tab
: {
  zstyle ':fzf-tab:complete:_zlua:*' query-string input
  # switch group using `,` and `.`
  zstyle ':fzf-tab:*' switch-group ',' '.'
  # preview directory's content with exa when completing cd
  zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always $realpath'
}
