# -*- mode: sh; eval: (sh-set-shell "zsh") -*-

# -------------------------------------
#  INITIALIZE
# -------------------------------------

# Check if a command exists
has() {
  which "$@" > /dev/null 2>&1
}

# Set up history.
export HISTSIZE=290000
export SAVEHIST=290000
export HISTFILE="${ZSH_DATA}/history"

export ZSH_RECENT_DIRS_FILE="${ZSH_CACHE}/chpwd-recent-dirs"
[[ ! -f "${ZSH_RECENT_DIRS_FILE}" ]] && \
  touch "${ZSH_RECENT_DIRS_FILE}"

umask 022

# Clone zgenom.
[[ -d "${ZGEN_SRC_DIR}" ]] || {
  git clone https://github.com/jandamm/zgenom.git "${ZGEN_SRC_DIR}"
}

# Load zgenom library.
source "${ZGEN_SRC_DIR}/zgenom.zsh"


# -------------------------------------
#  INSTANT P10K PROMPT + DIRENV
# -------------------------------------
#
# https://github.com/romkatv/powerlevel10k/#how-do-i-initialize-direnv-when-using-instant-prompt

# Begin direnv initialization immediately *before* p10k instant prompt loader
(( ${+commands[direnv]} )) && emulate zsh -c "$(direnv export zsh)"

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Finish direnv initialization immediately *after* p10k instant prompt loader
(( ${+commands[direnv]} )) && emulate zsh -c "$(direnv hook zsh)"

[[ ! -f "${ZDOTDIR}/.p10k.zsh" ]] || source "${ZDOTDIR}/.p10k.zsh"


# -------------------------------------
#  LOAD PLUGINS
# -------------------------------------

# Load plugins.
zgenom saved || {

  ZGEN_LOADED=()
  ZGEN_COMPLETIONS=()

  # Order is important here.
  zgenom load zsh-users/zsh-syntax-highlighting
  zgenom load zsh-users/zsh-history-substring-search

  zgenom load unixorn/autoupdate-zgenom

  # Don't forget aliases, or else.
  zgenom load djui/alias-tips

  zgenom load unixorn/git-extra-commands
  zgenom load skx/sysadmin-util

  # A smarter cd command
  zgenom load ajeetdsouza/zoxide

  # Because I haven't mastered the art of switching Node versions with Nix yet.
  zgenom load lukechilds/zsh-nvm

  zgenom load cantino/mcfly \
    mcfly.zsh

  # Interactively build jq expressions
  zgenom load reegnz/jq-zsh-plugin

  zgenom load hlissner/zsh-autopair \
    autopair.zsh

  zgenom load zsh-users/zsh-completions src

  # https://github.com/unixorn/zsh-quickstart-kit/blob/03516518551288b5adcb69d6eac8a1efe7f3812f/zsh/.zgen-setup#L159-L160
  export GENCOMPL_FPATH="${ZDOTDIR}/completions"

  # Automatic completion generator based on `--help` usage
  # https://github.com/dim-an/cod
  zgenom load dim-an/cod

  # Manually generate completions from a command's `--help` output
  zgenom load RobSis/zsh-completion-generator

  zgenom load Aloxaf/fzf-tab

  zgenom load zsh-users/zsh-autosuggestions

  zgenom load softmoth/zsh-vim-mode

  zgenom load romkatv/powerlevel10k powerlevel10k

  zgenom save

  # fd -uu \
  #   --extension zwc \
  #   '.' "$ZDOTDIR"
  #   --exec \
  #     rm '{}'

  zgenom compile $ZDOTDIR

}


# -------------------------------------
#  CONFIGURATION
# -------------------------------------

source "${ZDOTDIR}/config.zsh"

if [[ $TERM != dumb ]]; then
  . "${DOTFIELD_DIR}/lib/color.sh"

  source "${ZDOTDIR}/keybindings.zsh"
  source "${ZDOTDIR}/completion.zsh"
  source "${ZDOTDIR}/functions.zsh"
  source "${ZDOTDIR}/aliases.zsh"
  source "${ZDOTDIR}/fzf-tab.zsh"

  ## nix-generated
  source "${ZDOTDIR}/extra.zshrc"

  # Dedupe PATH.
  # https://til.hashrocket.com/posts/7evpdebn7g-remove-duplicates-in-zsh-path
  typeset -aU path;

  ##
  autopair-init
  enable-fzf-tab

fi

[[ -f "${ZDOTDIR}/config.local" ]] \
  && source "${ZDOTDIR}/config.local"
