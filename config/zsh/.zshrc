## zsh configuration

# ------------------------------------------------------------------------------
#   INITIALIZE
# ------------------------------------------------------------------------------

## Set up history.
export HISTSIZE=290000
export SAVEHIST=290000
export HISTFILE="${ZSH_DATA}/history"

## Autoload personal functions
if [[ -z ${fpath[(r)${ZDOTDIR}/functions]} ]] {
  fpath=("${ZDOTDIR}/functions" "${fpath[@]}")
}
autoload -Uz $fpath[1]/*(:t)

# TODO: what is this?
autoload -U zmv

export ZSH_RECENT_DIRS_FILE="${ZSH_CACHE}/chpwd-recent-dirs"
[[ ! -f "${ZSH_RECENT_DIRS_FILE}" ]] && \
  touch "${ZSH_RECENT_DIRS_FILE}"

# TODO: is this necessary/helpful? or is it ever set up elsewhere?
umask 022

## Load zinit.
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
source "${ZINIT_HOME}/zinit.zsh"

# Load all the auto-completions, has to be done before any compdefs.
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

## Color
. "${DOTFIELD_DIR}/lib/color.sh"

## Prompt
zinit ice depth=1
zinit light \
  romkatv/powerlevel10k

## Base Config
source "${ZDOTDIR}/config.zsh"
source "${ZDOTDIR}/functions.zsh"

## Nix-generated Config
source "${ZDOTDIR}/extra.zshrc"

# zt 0a pick'async.zsh' for \
#   mafredri/zsh-async


# ------------------------------------------------------------------------------
#   LOAD PLUGINS
# ------------------------------------------------------------------------------

## Aloxaf/fzf-tab
## https://github.com/Aloxaf/fzf-tab#configure
# -------------------------------------------

zt 0a atload'
  # `git checkout` => Disable sorting
  zstyle '':completion:*:git-checkout:*'' sort false

  # `cd` => Preview directory content during `cd` completion
  zstyle '':fzf-tab:complete:cd:*'' fzf-preview ''exa -1 --color=always --icons --all $realpath''

  # Display completion groups as headers
  zstyle '':fzf-tab:*'' show-group full

  # FIXME: choose better keys -- switching group with `.` means we can''t find hidden files
  # zstyle '':fzf-tab:*'' switch-group '','' ''.''

  # Prefix to indicate color
  zstyle '':fzf-tab:*'' prefix ''· ''

  # Use fzf-tab after second tab press
  # via https://github.com/mrksr/dotfiles/blob/e3dd8bde7d7be6a294f5a2a245cb7e4a15837d71/shell/.zshrc#L101-L113
  # fzf-tab-partial-and-complete() {
  #   if [[ $LASTWIDGET == ''fzf-tab-partial-and-complete'' ]]; then
  #     fzf-tab-complete
  #   else
  #     zle complete-word
  #   fi
  # }
  # zle -N fzf-tab-partial-and-complete
  # bindkey ''^I'' fzf-tab-partial-and-complete

  # Press tab again to accept the selected completion.
  zstyle '':fzf-tab:*'' fzf-bindings ''tab:accept''
' for \
  Aloxaf/fzf-tab

# trigger-load'!base16-fzf' \
# pick"bash/base16-${BASE16_THEME}.config" \
# nocompile'!' \
#   nicodebo/base16-fzf \
#


## Syntax Highlighting
## Completions
## Autosuggestions
# -------------------------------------------

zinit wait lucid for \
  silent atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
    zdharma-continuum/fast-syntax-highlighting \
  atload"!_zsh_autosuggest_start" \
    zsh-users/zsh-autosuggestions \
  as"completion" \
    zsh-users/zsh-completions \
  atload'!
    export HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND="bg=yellow,fg=white,bold"

    bindkey   "$terminfo[kcuu1]"  history-substring-search-up;
    bindkey   "^[[A"              history-substring-search-up;
    bindkey   -M vicmd "k"        history-substring-search-up;

    bindkey   "$terminfo[kcud1]"  history-substring-search-down;
    bindkey   "^[[B"              history-substring-search-down;
    bindkey   -M vicmd "j"        history-substring-search-down;
  ' \
    zsh-users/zsh-history-substring-search

# FIXME: move this
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8'

## History
## Navigation
## History Searching
# -------------------------------------------

# Load early for some helpful defaults to prevent an empty history stack for
# other plugins.
zt 0a for \
  OMZL::history.zsh

zt 0a for \
  ajeetdsouza/zoxide

zt 0a for \
  zdharma-continuum/history-search-multi-word

zinit ice \
  wait'!' \
zinit light zsh-users/zsh-history-substring-search

zinit light "lukechilds/zsh-nvm"


## zsh-vim-mode
## zsh-autopair
#  -------------------------------------------

zt 0b for \
  pick'autopair.zsh' \
  nocompletions \
  atload'autopair-init' \
    hlissner/zsh-autopair \
  softmoth/zsh-vim-mode


## Aliases
# -------------------------------------------

zt 0b from'gh-r' as'program' for \
  sei40kr/fast-alias-tips-bin \
  sei40kr/zsh-fast-alias-tips


## Completion Tools
# -------------------------------------------

# Automatic completion generator based on `--help` usage
# https://github.com/dim-an/cod
zt 2a for \
  dim-an/cod

# Manually generate completions from a command's `--help` output
zt 2a for \
  atload'zstyle :plugin:zsh-completion-generator programs grep' \
    RobSis/zsh-completion-generator


## Miscellaneous Tools
## -------------------------------------------

zt 2b for \
  OMZ::plugins/extract \
  agkozak/zhooks

eval "$(direnv hook zsh)"


## Custom Plugins
## -------------------------------------------

# FIXME: always says "5 seconds" and triggers notifications too frequently, even upon C-c
# . "${ZDOTDIR}/notify.zsh"

# ------------------------------------------------------------------------------
#  CONFIGURATION
# ------------------------------------------------------------------------------

if [[ $TERM != dumb ]]; then
  source "${ZDOTDIR}/keybindings.zsh"
  source "${ZDOTDIR}/completion.zsh"
  source "${ZDOTDIR}/aliases.zsh"
fi

[[ -f "${ZDOTDIR}/config.local" ]] \
  && source "${ZDOTDIR}/config.local"


# ------------------------------------------------------------------------------
#  CLEANUP
# ------------------------------------------------------------------------------

# Dedupe PATH.
# https://til.hashrocket.com/posts/7evpdebn7g-remove-duplicates-in-zsh-path
typeset -aU path;

# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
[[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh
