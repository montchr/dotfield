alias t='tail -f'

# Make it easy to copy/paste script commands verbatim
alias '$'=''

alias q=exit
alias clr=clear
alias sudo='sudo '
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias mkdir='mkdir -pv'
alias wget='wget -c'
alias path='echo -e ${PATH//:/\\n}'

alias rcpd='rcp --delete --delete-after'
alias rcpu='rcp --chmod=go='
alias rcpdu='rcpd --chmod=go='

# Use Kitty terminal's ssh helper kitten
alias sshk="kitty +kitten ssh -o SendEnv=CDOM_OS_APPEARANCE -A"
# Display an image in kitty
alias icat="kitty +kitten icat"

# Always enable colored `grep` output
# Note: `GREP_OPTIONS="--color=auto"` is deprecated, hence the alias usage.
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# Reload the shell (i.e. invoke as a login shell)
alias reload="exec $SHELL -l"


#=====================================
# Expand aliases inline
# http://blog.patshead.com/2012/11/automatically-expaning-zsh-global-aliases---simplified.html
#=====================================
function globalias() {
   if [[ $LBUFFER =~ ' [A-Z0-9]+$' ]]; then
     zle _expand_alias
     zle expand-word
   fi
   zle self-insert
}
zle -N globalias


# -------------------------------------
#  DIRECTORIES
# -------------------------------------

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'

# ls / exa
has exa && {
  alias exa="exa --color always --group-directories-first --git";
  alias l='exa --oneline'
  alias ll="exa -lg";
  alias la='exa -Fal'
  alias ld='exa -D1'
  alias lld='exa -Flagh --git --group-directories-first'
  alias tree='exa --tree'
}

# TODO: update for nix?
# alias ls=\"${BREW_PREFIX}/bin/gls\"

# Go to the top level of the current git repo.
alias cdg='cd `git rev-parse --show-toplevel`'


# -------------------------------------
#  GIT
# -------------------------------------

alias g="git"
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
# TODO: why? i think omz defines this too?
unalias gcp

# List all the commits on the current branch ahead of master.
alias glb="git log --oneline --decorate ${GIT_PRIMARY_BRANCH:-main}..${GIT_BRANCH_NAME:-HEAD}"

# Magical fix for all submodule issues.
alias gsumo='git submodule update --init --recursive'

# Show list of files changed in a specified commit or other ref
alias gdl="git diff-tree --no-commit-id --name-only -r"


# -------------------------------------
#  DARWIN/MACOS
# -------------------------------------

# IP addresses
alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
alias localip="ipconfig getifaddr en1"

# Flush DNS cache
alias flushdns="dscacheutil -flushcache"

# Empty the Trash on all mounted volumes and the main HDD
# Also, clear Apple’s System Logs to improve shell startup speed
alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl"
