# -------------------------------------
#  GENERAL
# -------------------------------------
# Most bindings provided by softmoth/zsh-vim-mode

# TODO: still necessary?
bindkey -v

# Do nothing on pageup and pagedown. Better than printing '~'.
bindkey -s '^[[5~' ''
bindkey -s '^[[6~' ''

# via @hlissner:
# https://github.com/hlissner/dotfiles/blob/1173284b76561d41edcb17062badccda012f7f2e/config/zsh/keybinds.zsh#L1-L5
# > The surround module wasn't working if KEYTIMEOUT was <= 10. Specifically,
# > (delete|change)-surround immediately abort into insert mode if KEYTIMEOUT <=
# > 8. If <= 10, then add-surround does the same. At 11, all these issues vanish.
# > Very strange!
export KEYTIMEOUT=15

# C-<space> to bypass completion
bindkey "^ " magic-space
# Normal space during searches
bindkey -M isearch " " magic-space

# history-substring-search
bindkey "$terminfo[kcuu1]" history-substring-search-up;
bindkey "$terminfo[kcud1]" history-substring-search-down;
bindkey "^[[A" history-substring-search-up;
bindkey "^[[B" history-substring-search-down;
bindkey -M vicmd "k" history-substring-search-up;
bindkey -M vicmd "j" history-substring-search-down;

# Shift + Tab
bindkey -M viins '^[[Z' reverse-menu-complete

# C-z to toggle current process (background/foreground)
# fancy-ctrl-z () {
#   if [[ $#BUFFER -eq 0 ]]; then
#     BUFFER="fg"
#     zle accept-line
#   else
#     zle push-input
#     zle clear-screen
#   fi
# }
# zle -N fancy-ctrl-z
# bindkey '^Z' fancy-ctrl-z

# -------------------------------------
#  WIDGETS
# -------------------------------------

# Expand aliases on <space>
bindkey " " globalias
