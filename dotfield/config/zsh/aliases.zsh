# -------------------------------------
#  DIRECTORIES
# -------------------------------------

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias l='exa --oneline'
alias ll='exa -Flagh --git'
alias la='exa -Fal'
# TODO: update for nix
# alias ls=\"${BREW_PREFIX}/bin/gls\"
alias lld='exa -Flagh --git --group-directories-first'
alias ld='exa -D1'
alias tree='exa --tree'


# -------------------------------------
#  GIT
# -------------------------------------

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
