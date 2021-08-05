# -*- mode: sh; eval: (sh-set-shell "zsh") -*-

# Check if a command exists
has() {
  which "$@" > /dev/null 2>&1
}

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

umask 022


# -------------------------------------
#  LOADING
# -------------------------------------

# Clone zgenom.
[[ -d "${ZGEN_SRC_DIR}" ]] || {
  git clone https://github.com/jandamm/zgenom.git "${ZGEN_SRC_DIR}"
}

# Load zgenom library.
source "${ZGEN_SRC_DIR}/zgenom.zsh"

# Load plugins.
zgenom saved || {

  ZGEN_LOADED=()
  ZGEN_COMPLETIONS=()

  zgenom load romkatv/powerlevel10k \
    powerlevel10k

  zgenom load "${ZDOTDIR}/.p10k.zsh"

  # Tab completions with an fzf frontend.
  zgenom load Aloxaf/fzf-tab

  [[ -z "$SSH_CONNECTION" ]] && {
    zgenom load zdharma/fast-syntax-highlighting
  }

  zgenom load zsh-users/zsh-history-substring-search

  zgenom load unixorn/autoupdate-zgenom

  # Don't forget aliases, or else.
  zgenom load djui/alias-tips

  zgenom load unixorn/git-extra-commands
  zgenom load skx/sysadmin-util

  # Jump around, faster.
  zgenom load skywind3000/z.lua

  # Because I haven't mastered the art of switching Node versions with Nix yet.
  zgenom load lukechilds/zsh-nvm

  zgenom load hlissner/zsh-autopair \
    autopair.zsh

  # TODO: not updated for experimental cli i.e. the one supporting flakes
  # zgenom load spwhitt/nix-zsh-completions

  zgenom load zsh-users/zsh-completions \
    src

  # Automatic completion generator based on `--help` usage
  # https://github.com/dim-an/cod
  zgenom load dim-an/cod

  # Manually generate completions from a command's `--help` output
  zgenom load RobSis/zsh-completion-generator

  zgenom load zsh-users/zsh-autosuggestions

  zgenom load softmoth/zsh-vim-mode

  zgenom save

  zgenom compile $ZDOTDIR

}

source "${ZDOTDIR}/config.zsh"
. "${DOTFIELD_DIR}/lib/color.sh"

if [[ $TERM != dumb ]]; then
  source "${ZDOTDIR}/keybindings.zsh"
  source "${ZDOTDIR}/completion.zsh"
  source "${ZDOTDIR}/functions.zsh"
  source "${ZDOTDIR}/aliases.zsh"
  source "${ZDOTDIR}/fzf-tab.zsh"

  ## nix-generated
  source $ZDOTDIR/extra.zshrc

  autopair-init
  enable-fzf-tab
fi

# Dedupe PATH.
# https://til.hashrocket.com/posts/7evpdebn7g-remove-duplicates-in-zsh-path
typeset -aU path;

# Load `base16-fzf` in each session (not with `zgenom save`) so we are using the
# most up-to-date color settings.
[[ -n "${BASE16_THEME}" ]] && {
  zgenom load fnune/base16-fzf \
    "bash/base16-${BASE16_THEME}.config"
}

# FIXME: move to a utility function and don't run on every session init
# export GIT_BRANCH_NAME="$(git symbolic-ref --short -q HEAD 2>/dev/null)"

## nix-generated
source $ZDOTDIR/extra.zshrc

[[ -f "${ZDOTDIR}/config.local" ]] \
  && source "${ZDOTDIR}/config.local"
