#!/usr/bin/env zsh

### :: COMPLETIONS :: FZF-TAB ::

# Preview directory contents
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always $realpath'

# Switch completion group using `,` and `.`
zstyle ':fzf-tab:*' switch-group ',' '.'
