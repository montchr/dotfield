# -*- mode: sh; eval: (sh-set-shell "zsh") -*-

# - - - - - - - - - - - - - - - - - - - -
# Instant Prompt
# - - - - - - - - - - - - - - - - - - - -

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# - - - - - - - - - - - - - - - - - - - -
# Zsh Core Configuration
# - - - - - - - - - - - - - - - - - - - -

HISTSIZE=1000000
SAVEHIST="${HISTSIZE}"
HISTFILE="${XDG_DATA_HOME}/zsh/history"

[[ -d "$CACHEDIR" ]] || mkdir -pv "$CACHEDIR"
[[ -d "$XDG_RUNTIME_DIR" ]] || mkdir -pv "$XDG_RUNTIME_DIR"

autoload -Uz add-zsh-hook

# Use chpwd_recent_dirs to start new sessions from last working dir
# Populate dirstack with chpwd history
autoload -Uz chpwd_recent_dirs
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-file "${TMPDIR}/chpwd-recent-dirs"
dirstack=($(awk -F"'" '{print $2}' ${$(zstyle -L ':chpwd:*' recent-dirs-file)[4]} 2>/dev/null))
[[ ${PWD} = ${HOME}  || ${PWD} = "." ]] && (){
  local dir
  for dir ($dirstack){
    [[ -d ${dir} ]] && { cd -q ${dir}; break }
  }
} 2>/dev/null

# Autoload personal functions.
fpath=(
  ${ZDOTDIR}/functions
  $fpath
)
# TODO: this doesn't exist!
# autoload -Uz ${ZDOTDIR}/functions/**/*(N:t)

# Configure zinit.
declare -A ZINIT
ZINIT[HOME_DIR]="${ZINIT_HOME}"
ZINIT[BIN_DIR]="${ZINIT_HOME}/bin"
ZINIT[PLUGINS_DIR]="${ZINIT_HOME}/plugins"
ZINIT[ZCOMPDUMP_PATH]="${ZSH_DATA}/zcompdump"

# Load zinit.
if [[ ! -f ${ZINIT_HOME}/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$ZINIT_HOME" && command chmod g-rwX "$ZINIT_HOME"
    command git clone https://github.com/zdharma/zinit "${ZINIT_HOME}/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi
source "${ZINIT_HOME}/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
  zinit-zsh/z-a-rust \
  zinit-zsh/z-a-as-monitor \
  zinit-zsh/z-a-patch-dl \
  zinit-zsh/z-a-bin-gem-node


#========================================
# Zinit turbo mode helper.
#
# Wrapper for `zinit` providing `depth'1'` and `lucid` as default ice mods.
#
# Usage:
#   zt [<wait-time>] [<ice-mods>...] <plugin>
#   zt 2e zdharma/fast-syntax-highlighting
#   zt 0a light-mode as'program' for ...
#   zt blockf for ...
# Parameters:
#   Wait time+suffix. Optional.
#   Other ice modifiers. Optional.
#   Plugin identifier.
#========================================
function zt() {
  zinit depth'1' lucid \
    ${1/#[0-9][a-e]/wait"$1"} \
    "${@:2}"
}

# - - - - - - - - - - - - - - - - - - - -
# Initial Prompt
# Annexes
# Config Source
# - - - - - - - - - - - - - - - - - - - -

zt light-mode for \
  pick'async.zsh' \
    mafredri/zsh-async \
  romkatv/powerlevel10k

# Load config files plugin.
zt light-mode for \
  _local/config

#
# Plugins
# - - - - - - - - - - - - - - - - - - - -

# Explanation:
# - History plugin is loaded early (as it has some defaults) to prevent empty history stack for other plugins
zt for \
  OMZL::history.zsh

zt 0a for \
  trigger-load"!base16-set-shell" \
  pick"scripts/base16-${BASE16_THEME}.sh" \
  nocompile \
    chriskempson/base16-shell

zt 0a for \
    atload"
        alias ..='cd ..'
        alias ...='cd ../..'
        alias ....='cd ../../..'
        alias .....='cd ../../../..'
        alias ......='cd ../../../../..'
        alias l='exa --oneline'
        alias ll='exa -Flagh --git'
        alias la='exa -Fal'
        # TODO: use nix path
        # alias ls=\"${BREW_PREFIX}/bin/gls\"
        alias lld='exa -Flagh --git --group-directories-first'
        alias ld='exa -D1'
        alias tree='exa --tree'
    " \
        OMZL::directories.zsh \
    OMZL::git.zsh \
    OMZL::grep.zsh \
    OMZL::termsupport.zsh \
    atload"
        alias gs='git status'
        alias gap='git add -p'
        alias gdt='git difftool'
        alias gca='git commit --amend'
        alias gcam='git commit --amend -C HEAD'
        alias grs='git reset'
        alias gcob='git checkout -b'
        alias gpl='git pull'
        alias gplo='git pull origin'
        alias gpls='git pull && gsumo'
        alias gps='git push'
        alias gpsu='git push -u'
        alias gpso='git push origin'
        alias gpss='git push && gsumo'
        alias gsh='git stash'
        alias gsha='git stash apply'
        alias gshp='git stash pop'
        alias gshl='git stash list'
        alias gl='git log --oneline --decorate -20'
        unalias gcp
    " \
        OMZP::git \
    OMZ::plugins/extract/extract.plugin.zsh \
    djui/alias-tips

zt 0a for \
  trigger-load'!base16-fzf' \
  pick"bash/base16-${BASE16_THEME}.config" \
  nocompile'!' \
    nicodebo/base16-fzf \
  atload'_zsh_autosuggest_start' \
    zsh-users/zsh-autosuggestions \
  if'false' ver'dev' \
    marlonrichert/zsh-autocomplete

zt 0b light-mode for \
  atload'export ENHANCD_FILTER=fzf:fzy:peco' \
    b4b4r07/enhancd \
  as'program' cp'wd.sh -> wd' mv'_wd.sh -> _wd' \
  atpull'!git reset --hard' \
  pick'wd' \
    mfaerevaag/wd \
  atload'
    bindkey "$terminfo[kcuu1]" history-substring-search-up;
    bindkey "$terminfo[kcud1]" history-substring-search-down;
    bindkey "^[[A" history-substring-search-up;
    bindkey "^[[B" history-substring-search-down;
    bindkey -M vicmd "k" history-substring-search-up;
    bindkey -M vicmd "j" history-substring-search-down;
  ' \
    zsh-users/zsh-history-substring-search \
  bindmap"^R -> ^H" atinit"
    zstyle :history-search-multi-word page-size 10
    zstyle :history-search-multi-word highlight-color fg=red,bold
    zstyle :plugin:history-search-multi-word reset-prompt-protect 1
  " \
    zdharma/history-search-multi-word \
  autoload'#manydots-magic' \
    knu/zsh-manydots-magic \
  pick'autopair.zsh' nocompletions atload'ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=(autopair-insert)' \
    hlissner/zsh-autopair \

zt 0b light-mode from'gh-r' as'program' for \
    sei40kr/fast-alias-tips-bin \
    sei40kr/zsh-fast-alias-tips


zt 0c light-mode atinit"
    typeset -gA FAST_HIGHLIGHT;
    FAST_HIGHLIGHT[git-cmsg-len]=100;
    ZINIT[COMPINIT_OPTS]=-C;
    zicompinit;
    zicdreplay;
  " for zdharma/fast-syntax-highlighting


zt 0c light-mode null for \
  sbin \
    paulirish/git-open \
  sbin'*/rm-trash' reset \
  patch"$pchf/%PLUGIN%.patch" \
    nateshmbhat/rm-trash

zt 2a for \
  atload"
    # Use fzf-tab after second tab press
    # via https://github.com/mrksr/dotfiles/blob/e3dd8bde7d7be6a294f5a2a245cb7e4a15837d71/shell/.zshrc#L101-L113
    fzf-tab-partial-and-complete() {
      if [[ \$LASTWIDGET = 'fzf-tab-partial-and-complete' ]]; then
        fzf-tab-complete
      else
        zle complete-word
      fi
    }

    zle -N fzf-tab-partial-and-complete
    bindkey '^I' fzf-tab-partial-and-complete
  " \
    Aloxaf/fzf-tab \
  zdharma/zui \
  zdharma/zbrowse

zt 2a light-mode for \
  OMZP::command-not-found \
  OMZP::composer \
  ael-code/zsh-colored-man-pages

zt 2b light-mode for \
  as"command" \
  pick"colortest" \
  atload"alias colortest=\"colortest base16-${BASE16_THEME}.sh\"" \
  blockf \
    chriskempson/base16-shell

zt 2b as'program' for \
  pick'revolver' \
    molovo/revolver \
  pick'zunit' \
  atclone'./build.zsh' atpull'%atclone' \
    molovo/zunit \
  pick"$ZPFX/bin/git-*" make"PREFIX=$ZPFX" nocompile light-mode \
    tj/git-extras \
  id-as'git-unique' pick'git-unique' \
    https://github.com/Osse/git-scripts/blob/master/git-unique


# - - - - - - - - - - - - - - - - - - - -
# Scripting Tools
# - - - - - - - - - - - - - - - - - - - -

zt 2b as'command' for \
  make"--directory=resources install PREFIX=$HOME/.local" \
    @matejak/argbash


# - - - - - - - - - - - - - - - - - - - -
# - Completions
# - Cleanup
# - - - - - - - - - - - - - - - - - - - -

zt 0e nocompile nocompletions for \
  @MenkeTechnologies/zsh-more-completions

zt 2d blockf for \
  atpull'zinit creinstall -q .' \
  atload"zicompinit; zicdreplay" \
    zsh-users/zsh-completions

zt 2e for \
  id-as'Cleanup' nocd \
  atinit'unset -f zt; _zsh_autosuggest_bind_widgets' \
    zdharma/null


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
