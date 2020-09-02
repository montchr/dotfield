# Directory navigation
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'

# Listing files
alias l='exa --all --oneline'
alias ll='exa -Flagh --git'
alias la='exa -Fal'
alias lld='exa -Flagh --git --group-directories-first'
alias ld='exa -D1'
alias tree='exa --tree'
alias ls='grc ls'

# Always enable colored `grep` output
# Note: `GREP_OPTIONS="--color=auto"` is deprecated, hence the alias usage.
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# Simple swapping
alias rm="trash"
alias search="fd"
alias cat="bat"

# Reload the shell (i.e. invoke as a login shell)
alias reload="exec $SHELL -l"

#
# Emacs
#

alias e='emacsclient -t'
alias ec='emacsclient -c'
alias vim='emacsclient -t'
alias vi='emacsclient -t'

#
# Git
#

alias g="git"
alias gs='scmpuff_status'

# commits / index
alias ga='git add'
alias gaa="git add --all"
alias gap="git add -p"
alias gdt="git difftool"
alias gc="git commit"
alias gca="git commit --amend"
# amend commit without editing message
alias gcam="git commit --amend -C HEAD"
alias gd='git diff'
alias grs='git reset'

# branches / refs
alias gsw='git switch'
alias gco='git checkout'
alias gcob="git checkout -b"
alias gb="git branch"
alias gm="git merge --no-edit"
alias gmt="git mergetool"
alias gcp="git cherry-pick"

# remotes
alias gpl="git pull"
alias gplo='git pull origin'
alias gpls="git pull && gsumo"
alias gps="git push"
alias gpsu="git push -u"
alias gpso="git push origin"
alias gpss="git push && gsumo"

# stashes
alias gsh="git stash"
alias gsha="git stash apply"
alias gshp="git stash pop"
alias gshl="git stash list"

# Show list of files changed in a commit
# Follow with commit hash
alias gdl="git diff-tree --no-commit-id --name-only -r $1"

# logs
alias gl="git log --oneline --decorate -20"
# List all the commits on the current branch ahead of master
alias glb="git log --oneline --decorate \$GIT_PRIMARY_BRANCH.."
# Based on https://stackoverflow.com/questions/1057564/pretty-git-branch-graphs
alias gla="git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(green)%aD%C(reset) %C(bold green)(%ar)%C(reset)%C(bold yellow)%d%C(reset)%n''          %C(bold cyan)%s%C(reset) %C(dim white)- %an%C(reset)%n''' --all"

alias gsumo="git submodule update --init --recursive"

#
# Node
#

alias lips="lorem-ipsum --copy"
alias lipsw="lorem-ipsum --copy 1 word"
alias lipss="lorem-ipsum --copy 1 sentence"
alias lipsp="lorem-ipsum --copy 1 paragraph"

#
# macOS
#

# IP addresses
alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
alias localip="ipconfig getifaddr en1"

# Flush DNS cache
alias flushdns="dscacheutil -flushcache"

# Empty the Trash on all mounted volumes and the main HDD
# Also, clear Apple’s System Logs to improve shell startup speed
alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl"

# Show/hide hidden files in Finder
alias showdotfiles="defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder"
alias hidedotfiles="defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder"

# Hide/show all desktop icons (useful when presenting)
alias showdeskicons="defaults write com.apple.finder CreateDesktop -bool true && killall Finder"
alias hidedeskicons="defaults write com.apple.finder CreateDesktop -bool false && killall Finder"

# Create spacer for the Dock
alias spacer="defaults write com.apple.dock persistent-apps -array-add '{tile-data={}; tile-type=\"spacer-tile\";}'; killall Dock"

# Launch Chrome with remote debugging support
alias chrome-debug="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --remote-debugging-port=9222"

# Pipe my public key to my clipboard.
alias pubkey="more ~/.ssh/id_rsa.pub | pbcopy | echo '=> Public key copied to pasteboard.'"
