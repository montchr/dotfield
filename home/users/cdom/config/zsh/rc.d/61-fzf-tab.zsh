#!/usr/bin/env zsh

### :: COMPLETIONS :: FZF-TAB ::

# Preview directory contents
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always $realpath'
# Border requires extra padding to prevent truncated results.
# <https://github.com/Aloxaf/fzf-tab/wiki/Configuration#fzf-pad>
if [[ "$FZF_DEFAULT_OPTS" =~ "--border" ]]; then
  zstyle ':fzf-tab:complete:*' fzf-pad '4'
fi


# Switch completion group using `,` and `.`
zstyle ':fzf-tab:*' switch-group ',' '.'


##: === KEYBINDINGS ================================================================================

# <space> to accept candidate
zstyle ':fzf-tab:*' fzf-bindings 'space:accept'


