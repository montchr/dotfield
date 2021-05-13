# Store homebrew prefix for later use to avoid re-executing
# TODO: only if brew is present!
# TODO: avoid! it's slow
# export BREW_PREFIX=$(brew --prefix)

# fzf
FZF_DEFAULT_OPTS="
--border
--height 40%
--extended
--ansi
--reverse
--cycle
--bind ctrl-s:toggle-sort
--bind 'alt-e:execute($EDITOR {} >/dev/tty </dev/tty)'
"
FZF_DEFAULT_COMMAND="fd --type f --hidden --follow --exclude .git 2>/dev/null"

# TODO: while the preview is nice in the terminal, it shouldn't be used as a default
# see fzf readme for more info: https://github.com/junegunn/fzf#preview-window
# --preview \"(bat --color=always {} || ls -l --color=always {}) 2>/dev/null | head -200\"
# --preview-window right:60%

# ls et al.
AUTO_LS_COMMANDS="exa --oneline"
AUTO_LS_NEWLINE=false

# Export variables when connected via SSH
# if [[ -n $SSH_CONNECTION ]]; then
#   # @TODO this might actually cause some issues with emacs in tty?
#   # export DISPLAY=:0
# fi
