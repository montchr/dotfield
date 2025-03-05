#!/usr/bin/env zsh

### :: COMPLETIONS :: FZF-TAB ::

# Set a minimum height for fzf.
# <https://github.com/Aloxaf/fzf-tab/wiki/Configuration#fzf-min-height>
zstyle ':fzf-tab:complete:*' fzf-min-height '4'

# Border requires extra padding to prevent truncated results.
# <https://github.com/Aloxaf/fzf-tab/wiki/Configuration#fzf-pad>
if [[ "$FZF_DEFAULT_OPTS" =~ "--border" ]]; then
  zstyle ':fzf-tab:complete:*' fzf-pad '4'
fi

# Only show group descriptions for groups with duplicate members.
# <https://github.com/Aloxaf/fzf-tab/wiki/Configuration#show-group>
zstyle ':fzf-tab:*' show-group brief

# Switch completion group using `,` and `.`
zstyle ':fzf-tab:*' switch-group ',' '.'


##: === KEYBINDINGS ================================================================================

# <space> to accept candidate
zstyle ':fzf-tab:*' fzf-bindings 'space:accept'


##: === CANDIDATE PREVIEWS  ========================================================================

# Preview directory contents
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'eza -1 --color=always $realpath'

# Show systemd unit status.
# <https://github.com/Aloxaf/fzf-tab/wiki/Preview#show-systemd-unit-status>
zstyle ':fzf-tab:complete:systemctl-*:*' fzf-preview 'SYSTEMD_COLORS=1 systemctl status $word'

# Show environment variable values.
# <https://github.com/Aloxaf/fzf-tab/wiki/Preview#environment-variable>
zstyle ':fzf-tab:complete:(-command-|-parameter-|-brace-parameter-|export|unset|expand):*' \
	fzf-preview 'echo ${(P)word}'

# Show `tldr` output.
# <https://github.com/Aloxaf/fzf-tab/wiki/Preview#tldr>
zstyle ':fzf-tab:complete:tldr:argument-1' fzf-preview 'tldr --color always $word'
