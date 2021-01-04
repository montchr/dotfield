# According to the Zsh Plugin Standard:
# http://zdharma.org/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html

0="${${ZERO:-${0:#$ZSH_ARGZERO}}:-${(%):-%N}}"
0="${${(M)0:#/*}:-$PWD/$0}"

# Then ${0:h} to get plugin's directory

# Autoload personal functions
fpath=("${0:h}/functions" "${fpath[@]}")
autoload -Uz $fpath[1]/*(.:t)

add-zsh-hook chpwd chpwd_ls

#########################
#       Variables       #
#########################

pchf="${0:h}/patches"
thmf="${0:h}/themes"
GENCOMPL_FPATH="${0:h}/completions"
WD_CONFIG="${ZPFX}/warprc"
ZSHZ_DATA="${ZPFX}/z"
AUTOENV_AUTH_FILE="${ZPFX}/autoenv_auth"
export CUSTOMIZEPKG_CONFIG="${HOME}/.config/customizepkg"

ZSH_AUTOSUGGEST_USE_ASYNC=true
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_HISTORY_IGNORE="?(#c100,)"
ZSH_AUTOSUGGEST_MANUAL_REBIND=set
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
FAST_ALIAS_TIPS_PREFIX="» $(tput setaf 6)"
FAST_ALIAS_TIPS_SUFFIX="$(tput sgr0) «"
HISTORY_SUBSTRING_SEARCH_FUZZY=set

export OPENCV_LOG_LEVEL=ERROR # Hide nonimportant errors for howdy
export rm_opts=(-I -v)

export \
  EDITOR="emacsclient -cn" \
  DOTS="${HOME}/.dots" \
  GIT_EDITOR="${EDITOR}" \
  GIT_PRIMARY_BRACH="main" \
  SYSTEMD_EDITOR=${EDITOR}


# GPG
# https://unix.stackexchange.com/questions/217737/pinentry-fails-with-gpg-agent-and-ssh
# export GPG_TTY='/usr/bin/tty'
# export SSH_AUTH_SOCK="$HOME/.gnupg/S.gpg-agent.ssh"

# SND
# export SND="$HOME/Music/0-sounds-0"
# export SNDS="$HOME/Music/0-sounds-0/-- samples --"
# export SNDBACKUPS="$HOME/Music/0-sounds-0/--- backup ---"

# nvm
export NODE_VERSIONS="~/.nvm/versions/node/"
export NODE_VERSION_PREFIX='v'
export NVM_SYMLINK_CURRENT='true'

# Personal preferences.
export \
  CDOM_DOOM_EMACS_THEME_DARK="doom-monokai-pro" \
  CDOM_DOOM_EMACS_THEME_LIGHT="doom-plain"

FZF_DEFAULT_OPTS="
--border
--height 80%
--extended
--ansi
--reverse
--cycle
--bind ctrl-s:toggle-sort
--bind 'alt-e:execute($EDITOR {} >/dev/tty </dev/tty)'
--preview \"(bat --color=always {} || ls -l --color=always {}) 2>/dev/null | head -200\"
--preview-window right:60%
"
FZF_DEFAULT_COMMAND="fd --type f --hidden --follow --exclude .git 2>/dev/null"

AUTO_LS_COMMANDS="colorls"
AUTO_LS_NEWLINE=false

FZ_HISTORY_CD_CMD=zshz
ZSHZ_CMD="" # Don't set the alias, fz will cover that
ZSHZ_UNCOMMON=1
# forgit_ignore="/dev/null" #replaced gi with local git-ignore plugin


# Export variables when connected via SSH
if [[ -n $SSH_CONNECTION ]]; then
  export DISPLAY=:0
  alias ls="lsd --group-dirs=first --icon=never"
else
  alias ls='lsd --group-dirs=first'
fi


#########################
#       Aliases         #
#########################

# Access zsh config files
alias zshconf="(){ setopt extendedglob local_options; e ${HOME}/.zshrc ${0:h}/config-files.plugin.zsh ${0:h}/themes/\${MYPROMPT}-*~*.zwc }"

alias e='emacsclient -t'
alias ec='emacsclient -cn'
alias g="git"
alias t='tail -f'

# Directory navigation
alias ..='cd .. 2>/dev/null || cd "$(dirname $PWD)"' # Allows leaving from deleted directories
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
# alias rm="trash" # macOS only
alias search="fd"
alias cat="bat"

# Reload the shell (i.e. invoke as a login shell)
alias reload="exec $SHELL -l"


#
# Git
#

# @TODO: many of these need to be functions
# alias gs='scmpuff_status'

# commits / index
# alias ga="git add"
# alias gaa="git add --all"
# alias gap="git add -p"
# alias gdt="git difftool"
# alias gc="git commit"
# alias gca="git commit --amend"
# # amend commit without editing message
# alias gcam="git commit --amend -C HEAD"
# alias gd='git diff'
# alias grs='git reset'

# branches / refs
# alias gsw='git switch'
# alias gco='git checkout'
# alias gcob="git checkout -b"
# alias gb="git branch"
# alias gm="git merge --no-edit"
# alias gmt="git mergetool"
# alias gcp="git cherry-pick"

# remotes
# alias gpl="git pull"
# alias gplo='git pull origin'
# alias gpls="git pull && gsumo"
# alias gps="git push"
# alias gpsu="git push -u"
# alias gpso="git push origin"
# alias gpss="git push && gsumo"

# stashes
# alias gsh="git stash"
# alias gsha="git stash apply"
# alias gshp="git stash pop"
# alias gshl="git stash list"

# Show list of files changed in a commit
# Follow with commit hash
# alias gdl="git diff-tree --no-commit-id --name-only -r $1"

# logs
# alias gl="git log --oneline --decorate -20"
# List all the commits on the current branch ahead of master
# alias glb="git log --oneline --decorate \$GIT_PRIMARY_BRANCH.."
# Based on https://stackoverflow.com/questions/1057564/pretty-git-branch-graphs
# alias gla="git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(green)%aD%C(reset) %C(bold green)(%ar)%C(reset)%C(bold yellow)%d%C(reset)%n''          %C(bold cyan)%s%C(reset) %C(dim white)- %an%C(reset)%n''' --all"

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

#########################
#         Other         #
#########################

bindkey -v                  # VI bindings
setopt append_history       # Allow multiple terminal sessions to all append to one zsh command history
setopt hist_ignore_all_dups # delete old recorded entry if new entry is a duplicate.
setopt no_beep              # don't beep on error
setopt auto_cd              # If you type foo, and it isn't a command, and it is a directory in your cdpath, go there
setopt multios              # perform implicit tees or cats when multiple redirections are attempted
setopt prompt_subst         # enable parameter expansion, command substitution, and arithmetic expansion in the prompt
setopt interactive_comments # Allow comments even in interactive shells (especially for Muness)
setopt pushd_ignore_dups    # don't push multiple copies of the same directory onto the directory stack
setopt auto_pushd           # make cd push the old directory onto the directory stack
setopt pushdminus           # swapped the meaning of cd +1 and cd -1; we want them to mean the opposite of what they mean

# Fuzzy matching of completions for when you mistype them:
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3>7?7:($#PREFIX+$#SUFFIX)/3))numeric)'

# Pretty completions
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:corrections' format ' %F{green}-- %d (errors: %e) --%f'
zstyle ':completion:*:descriptions' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec))'
zstyle ':completion:*' use-cache true
zstyle ':completion:*' rehash true

bindkey -s '^[[5~' ''            # Do nothing on pageup and pagedown. Better than printing '~'.
bindkey -s '^[[6~' ''
