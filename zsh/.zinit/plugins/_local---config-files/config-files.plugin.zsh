# According to the Zsh Plugin Standard:
# http://zdharma.org/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html

0="${${ZERO:-${0:#$ZSH_ARGZERO}}:-${(%):-%N}}"
0="${${(M)0:#/*}:-$PWD/$0}"

# Then ${0:h} to get plugin's directory

# Autoload personal functions
# (Except those beginning with an underscore)
fpath=("${0:h}/functions" "${fpath[@]}")
autoload -Uz $fpath[1]/^_*(:t)

add-zsh-hook chpwd chpwd_ls


# - - - - - - - - - - - - - - - - - - - -
# Variables
# - - - - - - - - - - - - - - - - - - - -

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

#
# PATH Modifications
#

export PATH="$DOTS/bin:$PATH"

# Doom Emacs
export PATH="$HOME/.emacs.d/bin:$PATH"

# GPG
# https://unix.stackexchange.com/questions/217737/pinentry-fails-with-gpg-agent-and-ssh
# export GPG_TTY='/usr/bin/tty'
# export SSH_AUTH_SOCK="$HOME/.gnupg/S.gpg-agent.ssh"

# nvm / node
export NVM_SYMLINK_CURRENT='true'
export NVM_DIR="$HOME/.nvm"
export NVM_LAZY_LOAD=true
export NODE_VERSIONS="~/.nvm/versions/node/"
export NODE_VERSION_PREFIX='v'

# Personal preferences.
export \
  GIT_PRIMARY_BRANCH="main" \
  CDOM_DOOM_EMACS_THEME_DARK="base16-black-metal-bathory" \
  CDOM_DOOM_EMACS_THEME_LIGHT="doom-plain" \
  YABAI_SCRIPTS="$DOTS/yabai/scripts"


FZF_DEFAULT_OPTS="
--border
--height 80%
--extended
--ansi
--reverse
--cycle
--bind ctrl-s:toggle-sort
--bind 'alt-e:execute($EDITOR {} >/dev/tty </dev/tty)'
"
FZF_DEFAULT_COMMAND="fd --type f --hidden --follow --exclude .git 2>/dev/null"

# @TODO while the preview is nice in the terminal, it shouldn't be used as a default
# this may have caused emacs counsel-file to crash when attempting to load previews for particularly heavy binary files
# see fzf readme for more info: https://github.com/junegunn/fzf#preview-window
# --preview \"(bat --color=always {} || ls -l --color=always {}) 2>/dev/null | head -200\"
# --preview-window right:60%


AUTO_LS_COMMANDS="exa --oneline"
AUTO_LS_NEWLINE=false

forgit_ignore="/dev/null" #replaced gi with local git-ignore plugin


# Export variables when connected via SSH
if [[ -n $SSH_CONNECTION ]]; then
  # @TODO this might actually cause some issues with emacs in tty?
  # export DISPLAY=:0
  alias ls="exa --group-directories-first"
else
  alias ls="exa --group-directories-first --icons"
fi


# - - - - - - - - - - - - - - - - - - - -
# Aliases
# - - - - - - - - - - - - - - - - - - - -

alias e='emacsclient -t'
alias ec='emacsclient -cn'
alias g="git"
alias t='tail -f'
alias sshk='kitty +kitten ssh'

# Directory navigation

# Listing files
alias l='exa --oneline'
alias ll='exa -Flagh --git'
alias la='exa -Fal'
alias lld='exa -Flagh --git --group-directories-first'
alias ld='exa -D1'
alias tree='exa --tree'

# Always enable colored `grep` output
# Note: `GREP_OPTIONS="--color=auto"` is deprecated, hence the alias usage.
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# Simple swapping
alias cat="bat"

# Reload the shell (i.e. invoke as a login shell)
alias reload="exec $SHELL -l"


#
# Git
# @TODO: many of these need to be functions
#


# List all the commits on the current branch ahead of master
alias glb='git log --oneline --decorate \$GIT_PRIMARY_BRANCH..'

# Based on https://stackoverflow.com/questions/1057564/pretty-git-branch-graphs
# alias gla='git log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(green)%aD%C(reset) %C(bold green)(%ar)%C(reset)%C(bold yellow)%d%C(reset)%n''          %C(bold cyan)%s%C(reset) %C(dim white)- %an%C(reset)%n''' --all'

# magical fix for all submodule issues
alias gsumo='git submodule update --init --recursive'

# Show list of files changed in a commit
# Follow with commit hash
# alias gdl="git diff-tree --no-commit-id --name-only -r $1"


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


# - - - - - - - - - - - - - - - - - - - -
# Miscellaneous
# - - - - - - - - - - - - - - - - - - - -

bindkey -v        # Vi bindings
bindkey "^A"      beginning-of-line     "^E"      end-of-line
bindkey "^?"      backward-delete-char  "^H"      backward-delete-char
bindkey "^W"      backward-kill-word    "\e[1~"   beginning-of-line
bindkey "\e[7~"   beginning-of-line     "\e[H"    beginning-of-line
bindkey "\e[4~"   end-of-line           "\e[8~"   end-of-line
bindkey "\e[F"    end-of-line           "\e[3~"   delete-char
bindkey "^J"      accept-line           "^M"      accept-line
bindkey "^T"      accept-line           "^R"      history-incremental-search-backward

# Use case-insensitve globbing.
unsetopt case_glob
# glob dotfiles as well
setopt globdots
# use extended globbing
setopt extendedglob
# Automatically change directory if a directory is entered
setopt autocd
# Allow brace character class list expansion.
setopt brace_ccl
# Combine zero-length punctuation characters (accents) with the base character.
setopt combining_chars
# Allow 'Henry''s Garage' instead of 'Henry'\''s Garage'.
setopt rc_quotes
# Don't print a warning message if a mail file has been accessed.
unsetopt mail_warning

#
# Jobs
#

# List jobs in the long format by default.
setopt long_list_jobs
# Attempt to resume existing job before creating a new process.
setopt auto_resume
# Report status of background jobs immediately.
setopt notify
# Don't run all background jobs at a lower priority.
unsetopt bg_nice
# Don't kill jobs on shell exit.
unsetopt hup
# Don't report on jobs when shell exit.
unsetopt check_jobs
# Turn on corrections
setopt correct

#
# Completion Options
#

# Complete from both ends of a word.
setopt complete_in_word
# Move cursor to the end of a completed word.
setopt always_to_end
# Perform path search even on command names with slashes.
setopt path_dirs
# Show completion menu on a successive tab press.
setopt auto_menu
# Automatically list choices on ambiguous completion.
setopt auto_list
# Do not autoselect the first completion entry.
# setopt menu_complete
# Disable start/stop characters in shell editor.
# unsetopt flow_control



setopt append_history       # Allow multiple terminal sessions to all append to one zsh command history
setopt hist_ignore_all_dups # delete old recorded entry if new entry is a duplicate.
setopt no_beep              # don't beep on error
setopt multios              # perform implicit tees or cats when multiple redirections are attempted
setopt prompt_subst         # enable parameter expansion, command substitution, and arithmetic expansion in the prompt
setopt interactive_comments # Allow comments even in interactive shells
setopt pushd_ignore_dups    # don't push multiple copies of the same directory onto the directory stack
setopt auto_pushd           # make cd push the old directory onto the directory stack
setopt pushdminus           # swapped the meaning of cd +1 and cd -1; we want them to mean the opposite of what they mean

# Fuzzy matching of completions for when you mistype them:
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3>7?7:($#PREFIX+$#SUFFIX)/3))numeric)'

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
zstyle ':fzf-tab:complete:_zlua:*' query-string input
zstyle ':fzf-tab:complete:cd:*' extra-opts --preview=$extract'exa -1 --color=always ${~ctxt[hpre]}$in'
zstyle ':fzf-tab:complete:kill:argument-rest' extra-opts --preview=$extract'ps --pid=$in[(w)1] -o cmd --no-headers -w -w' --preview-window=down:3:wrap

bindkey -s '^[[5~' ''            # Do nothing on pageup and pagedown. Better than printing '~'.
bindkey -s '^[[6~' ''
