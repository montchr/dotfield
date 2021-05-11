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

export COMPLETION_WAITING_DOTS="true"

# Correct spelling for commands
setopt correct

# turn off the infernal correctall for filenames
unsetopt correctall

if has brew; then
  BREW_PREFIX=$(brew --prefix)
  if [[ -d "${BREW_PREFIX}/bin"]]; then
    export PATH="$PATH:${BREW_PREFIX}/bin"
  fi
  if [[ -d "${BREW_PREFIX}/sbin"]]; then
    export PATH="$PATH:${BREW_PREFIX}/sbin"
  fi
fi

# TODO: sync with color schemes?
# http://geoff.greer.fm/lscolors/
export LSCOLORS='Exfxcxdxbxegedabagacad'
export LS_COLORS='di=1;34;40:ln=35;40:so=32;40:pi=33;40:ex=31;40:bd=34;46:cd=34;43:su=0;41:sg=0;46:tw=0;42:ow=0;43:'

# Summon zgenom.
[[ -d "${ZGEN_SRC_DIR}" ]] || {
  git clone https://github.com/jandamm/zgenom.git "${ZGEN_SRC_DIR}"
}

source "$ZGENOM_PARENT_DIR/zgenom/zgenom.zsh"

ZGEN_LOADED=()
ZGEN_COMPLETIONS=()

zgenom saved || {
  # Order matters here!
  # 1. zsh-users/zsh-syntax-highlighting
  # 2. zsh-users/zsh-history-substring-search
  zgenom load zsh-users/zsh-syntax-highlighting
  zgenom load zsh-users/zsh-history-substring-search

  # Set keystrokes for substring searching
  zmodload zsh/terminfo
  bindkey "$terminfo[kcuu1]" history-substring-search-up
  bindkey "$terminfo[kcud1]" history-substring-search-down

  zgenom load unixorn/autoupdate-zgenom

  # Colorize command output.
  zgenom load unixorn/warhol.plugin.zsh

  zgenom load ael-code/zsh-colored-man-pages
  zgenom load chriskempson/base16-shell

  # @unixorn's macOS helpers.
  zgenom load unixorn/tumult.plugin.zsh

  zgenom load djui/alias-tips
  zgenom load unixorn/git-extra-commands
  zgenom load unixorn/fzf-zsh-plugin
  zgenom load skx/sysadmin-util

  # Aliases for working with current repo on GitHub.
  zgenom load peterhurford/git-it-on.zsh

  # Encrypt some repo files.
  zgenom load StackExchange/blackbox

  # Load some oh-my-zsh plugins
  zgenom oh-my-zsh plugins/pip
  zgenom oh-my-zsh plugins/sudo
  zgenom oh-my-zsh plugins/aws
  zgenom oh-my-zsh plugins/chruby
  zgenom oh-my-zsh plugins/colored-man-pages
  zgenom oh-my-zsh plugins/git
  zgenom oh-my-zsh plugins/github
  zgenom oh-my-zsh plugins/python
  zgenom oh-my-zsh plugins/rsync
  zgenom oh-my-zsh plugins/screen
  zgenom oh-my-zsh plugins/vagrant

  if [[ $(uname -a | grep -ci Darwin) == 1 ]]; then
    # Load macOS-specific plugins
    zgenom oh-my-zsh plugins/brew
    zgenom oh-my-zsh plugins/osx
  fi

  # TODO: barely does anything -- why not just copy?
  zgenom load chrissicool/zsh-256color

  zgenom load hlissner/zsh-autopair

  zgenom load zsh-users/zsh-completions src
  zgenom load srijanshetty/docker-zsh

  # Load this last!
  GENCOMPL_FPATH=${ZDOTDIR}/complete

  # Generate completions.
  zgenom load RobSis/zsh-completion-generator

  zgenom load zsh-users/zsh-autosuggestions

  # Prompt
  zgenom load romkatv/powerlevel10k powerlevel10k

  zgenom save
}


# -------------------------------------
#  HISTORY
# -------------------------------------

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt hist_save_no_dups
setopt hist_verify
setopt INC_APPEND_HISTORY
unsetopt HIST_BEEP

# Share history between all sessions.
setopt share_history
#setopt noclobber

HISTSIZE=1000000
SAVEHIST=1000000
HISTFILE=$ZSH_DATA/history

export HISTIGNORE="ls:cd:cd -:pwd:exit:date:* --help"


# -------------------------------------
#  DIRECTORIES
# -------------------------------------

setopt pushd_ignore_dups
setopt AUTO_CD


# -------------------------------------
#  COMPLETIONS
# -------------------------------------

# Completions settings:
setopt ALWAYS_TO_END     # Move cursor to the end of a completed word.
setopt AUTO_LIST         # Automatically list choices on ambiguous completion.
setopt AUTO_MENU         # Show completion menu on a successive tab press.
setopt AUTO_PARAM_SLASH  # If completed parameter is a directory, add a trailing slash.
setopt COMPLETE_IN_WORD  # Complete from both ends of a word.
unsetopt MENU_COMPLETE   # Do not autoselect the first completion entry.

# Enable comments in interactive shell.
setopt INTERACTIVE_COMMENTS
# Enable more powerful glob features
setopt extended_glob

# Stop TRAMP (in Emacs) from hanging or term/shell from echoing back commands
# https://github.com/hlissner/dotfiles/blob/1173284b76561d41edcb17062badccda012f7f2e/config/zsh/config.zsh#L1-L7
if [[ $TERM == dumb || -n $INSIDE_EMACS ]]; then
  unsetopt zle prompt_cr prompt_subst
  whence -w precmd >/dev/null && unfunction precmd
  whence -w preexec >/dev/null && unfunction preexec
  PS1='$ '
fi


# -------------------------------------
#  TIME + UPDATES
# -------------------------------------

# Long running processes should return time (in seconds) when they complete.
REPORTTIME=2
TIMEFMT="%U user %S system %P cpu %*Es total"

# Disable Oh-My-ZSH's internal updating -- we handle our own updates.
DISABLE_AUTO_UPDATE=true


# -------------------------------------
#  ALIASES
# -------------------------------------

. $ZDOTDIR/aliases.zsh

# Expand aliases inline
# http://blog.patshead.com/2012/11/automatically-expaning-zsh-global-aliases---simplified.html
function globalias() {
   if [[ $LBUFFER =~ ' [A-Z0-9]+$' ]]; then
     zle _expand_alias
     zle expand-word
   fi
   zle self-insert
}

zle -N globalias

bindkey " " globalias
bindkey "^ " magic-space           # control-space to bypass completion
bindkey -M isearch " " magic-space # normal space during searches

# TODO: use dotfield msg utils
# deal with screen, if we're using it - courtesy MacOSXHints.com
# Login greeting ------------------
# if [ "$TERM" = "screen" -a ! "$SHOWED_SCREEN_MESSAGE" = "true" ]; then
#   detached_screens=$(screen -list | grep Detached)
#   if [ ! -z "$detached_screens" ]; then
#     echo "+---------------------------------------+"
#     echo "| Detached screens are available:       |"
#     echo "$detached_screens"
#     echo "+---------------------------------------+"
#   fi
# fi

# TODO: grc looks nice but:
# 1. move this elsewhere
# 2. make it work with nix
#
# # grc colorizes the output of a lot of commands. If the user installed it,
# # use it.

# # Try and find the grc setup file
# if (( $+commands[grc] )); then
#   GRC_SETUP='/usr/local/etc/grc.bashrc'
# fi
# if (( $+commands[grc] )) && (( $+commands[brew] ))
# then
#   GRC_SETUP="$(brew --prefix)/etc/grc.bashrc"
# fi
# if [[ -r "$GRC_SETUP" ]]; then
#   source "$GRC_SETUP"
# fi
# unset GRC_SETUP

# if (( $+commands[grc] ))
# then
#   function ping5(){
#     grc --color=auto ping -c 5 "$@"
#   }
# else
#   alias ping5='ping -c 5'
# fi


# -------------------------------------
#  CLEANUP + OPTIMIZATION
# -------------------------------------

# Speed up autocomplete, force prefix mapping
zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle -e ':completion:*:default' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)*==34=34}:${(s.:.)LS_COLORS}")';

# Load any custom zsh completions we've installed
# TODO: load from zdotdir/completions


# TODO: read up on what is this?
autoload -U zmv

# Dedupe PATH.
# https://til.hashrocket.com/posts/7evpdebn7g-remove-duplicates-in-zsh-path
typeset -aU path;



ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=(bracketed-paste)
ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=(autopair-insert)









# - - - - - - - - - - - - - - - - - - - -
# Miscellaneous
# - - - - - - - - - - - - - - - - - - - -

## Auto-generated by my nix config
source $ZDOTDIR/extra.zshrc

zinit is-snippet for \
  if"[[ -f "${ZDOTDIR}/config.local" ]]" \
    "${ZDOTDIR}/config.local"

[[ -f "${ZDOTDIR}/.p10k.zsh" ]] \
  && source "${ZDOTDIR}/.p10k.zsh"
