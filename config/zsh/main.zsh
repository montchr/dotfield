# Personal Zsh configuration file. It is strongly recommended to keep all
# shell customization and configuration (including exported environment
# variables such as PATH) in this file or in files sourced from it.
#
# Documentation: https://github.com/romkatv/zsh4humans/blob/v5/README.md.

###: INIT Z4H :=================================================================

# Periodic auto-update on Zsh startup: 'ask' or 'no'.
# You can manually run `z4h update` to update everything.
zstyle ':z4h:' auto-update      'ask'
# Ask whether to auto-update this often; has no effect if auto-update is 'no'.
zstyle ':z4h:' auto-update-days '7'

# Keyboard type: 'mac' or 'pc'.
# FIXME: change based on OS .. perhaps move to extraInit?
zstyle ':z4h:bindkey' keyboard 'mac'

# Mark up shell's output with semantic information.
zstyle ':z4h:' term-shell-integration 'yes'

# Right-arrow key accepts one character ('partial-accept') from
# command autosuggestions or the whole thing ('accept')?
zstyle ':z4h:autosuggestions' forward-char 'accept'

# Recursively traverse directories when TAB-completing files.
zstyle ':z4h:fzf-complete' recurse-dirs 'yes'

# Enable direnv to automatically source .envrc files.
zstyle ':z4h:direnv'         enable 'yes'
# Show "loading" and "unloading" notifications from direnv.
zstyle ':z4h:direnv:success' notify 'yes'

# Enable ('yes') or disable ('no') automatic teleportation of z4h over
# SSH when connecting to these hosts.
zstyle ':z4h:ssh:example-hostname1'   enable 'yes'
zstyle ':z4h:ssh:*.example-hostname2' enable 'no'
# The default value if none of the overrides above match the hostname.
zstyle ':z4h:ssh:*'                   enable 'no'

# Send these files over to the remote host when connecting over SSH to the
# enabled hosts.
# zstyle ':z4h:ssh:*' send-extra-files '~/.nanorc' '~/.env.zsh'

# Clone additional Git repositories from GitHub.
#
# This doesn't do anything apart from cloning the repository and keeping it
# up-to-date. Cloned files can be used after `z4h init`.
z4h install ohmyzsh/ohmyzsh || return
z4h install ajeetdsouza/zoxide || return
z4h install nvm-sh/nvm || return

# Install or update core components (fzf, zsh-autosuggestions, etc.) and
# initialize Zsh. After this point console I/O is unavailable until Zsh
# is fully initialized. Everything that requires user interaction or can
# perform network I/O must be done above. Everything else is best done below.
z4h init || return

###: ENVIRONMENT VARIABLES :====================================================
#==: in general, these should be configured by nix

###: LOAD SOURCES :=============================================================

##: init
z4h source $DOTFIELD_DIR/lib/color.sh

##: local config files
z4h source $ZDOTDIR/config.local
z4h source $ZDOTDIR/extra.zshrc


###: LOAD PLUGINS :=============================================================
# Use additional Git repositories pulled in with `z4h install`.

z4h load ajeetdsouza/zoxide
z4h source nvm-sh/nvm/nvm.sh


###: KEYBINDINGS :==============================================================

z4h bindkey undo Ctrl+/   Shift+Tab  # undo the last command line change
z4h bindkey redo Option+/            # redo the last undone command line change

z4h bindkey z4h-cd-back    Shift+Left   # cd into the previous directory
z4h bindkey z4h-cd-forward Shift+Right  # cd into the next directory
z4h bindkey z4h-cd-up      Shift+Up     # cd into the parent directory
z4h bindkey z4h-cd-down    Shift+Down   # cd into a child directory


###: FUNCTIONS :================================================================

autoload -Uz zmv

function md() { [[ $# == 1 ]] && mkdir -p -- "$1" && cd -- "$1" }
compdef _directories md

# Define named directories: ~w <=> Windows home directory on WSL.
[[ -z $z4h_win_home ]] || hash -d w=$z4h_win_home

# ls on cd
chpwd_ls() { exa --group-directories-first; }
add-zsh-hook chpwd chpwd_ls

# Start new sessions from most recent working directory.
# FIXME: this hasn't been working since moving to z4h
# autoload -Uz chpwd_recent_dirs
# add-zsh-hook chpwd chpwd_recent_dirs

# Populate dirstack with chpwd history.
zstyle ':chpwd:*' recent-dirs-file "${ZSH_RECENT_DIRS_FILE}"
dirstack=($(awk -F"'" '{print $2}' ${$(zstyle -L ':chpwd:*' recent-dirs-file)[4]} 2>/dev/null))
[[ "${PWD}" == "${HOME}" || "${PWD}" == "." ]] && () {
  local dir
  for dir ($dirstack) {
    [[ -d "${dir}" ]] && { cd -q "${dir}"; break }
  }
} 2>/dev/null


##: COLOR {{

#=====================================
# Print the current shell's color palette.
#
# Outputs:
#   Available colors
#=====================================
function color::palette() {
    local -a colors
    for i in {000..255}; do
        colors+=("%F{$i}$i%f")
    done
    print -cP $colors
}


#=====================================
# Print an escape sequence for a given color code.
#
# Usage:
#   color::print <color-code>
# Parameters:
#   Color code
# Outputs:
#   Escape sequence
#=====================================
function color::print() {
    local color="%F{$1}"
    echo -E ${(qqqq)${(%)color}}
}

##: }}


###: ALIASES :==================================================================

alias tree='tree -a -I .git'

# Make it easy to copy/paste script commands verbatim
alias '$'=''

# Add flags to existing aliases.
alias ls="${aliases[ls]:-ls} -A"


###: SHELL OPTIONS :============================================================

# Long running processes should return time (in seconds) when they complete.
REPORTTIME=2
TIMEFMT="%U user %S system %P cpu %*Es total"

# Stop TRAMP (in Emacs) from hanging or term/shell from echoing back commands
# https://github.com/hlissner/dotfiles/blob/1173284b76561d41edcb17062badccda012f7f2e/config/zsh/config.zsh#L1-L7
if [[ $TERM == dumb || -n $INSIDE_EMACS ]]; then
  unsetopt zle prompt_cr prompt_subst
  whence -w precmd >/dev/null && unfunction precmd
  whence -w preexec >/dev/null && unfunction preexec
  PS1='$ '
fi

# Treat these characters as part of a word.
WORDCHARS='_-*?[]~&.;!#$%^(){}<>'

# ls et al.
AUTO_LS_COMMANDS="exa --oneline"
AUTO_LS_NEWLINE=false


##: INTERACTION/FEEDBACK {{

# setopt HASH_LIST_ALL
# setopt IGNOREEOF

# setopt GLOBDOTS                 # Glob dotfiles as well.
# setopt COMBINING_CHARS          # Combine zero-length punc chars (accents) with base char
# setopt EXTENDED_GLOB            # Enable more powerful glob features
setopt INTERACTIVE_COMMENTS     # Enable comments in interactive shell.
setopt RC_QUOTES                # Allow 'Henry''s Garage' instead of 'Henry'\''s Garage'

# unsetopt BEEP             # No BEEP!
# unsetopt BRACE_CCL        # Allow brace character class list expansion.
# unsetopt CASE_GLOB        # Use case-insensitve globbing.

##: }}


##: DIRECTORIES {{

setopt AUTO_CD              # Auto changes to a directory without typing cd.
setopt AUTO_PUSHD           # Make cd push the old directory onto the directory stack
setopt PUSHD_IGNORE_DUPS    # Don't push multiple copies of the same directory onto the directory stack.
# setopt CDABLE_VARS          # Change directory to a path stored in a variable.
# setopt PUSHD_SILENT         # Do not print the directory stack after pushd or popd.
# setopt PUSHD_TO_HOME        # Push to home directory when no argument is given.
# setopt PUSHDMINUS           # Swap the meaning of cd +1 and cd -1; we want them to mean the opposite of what they mean.

# unsetopt AUTO_NAME_DIRS     # Don't add variable-stored paths to ~ list

##: }}


##: PROCESSES {{

setopt AUTO_RESUME        # Attempt to resume existing job before creating a new process.
setopt LONG_LIST_JOBS     # List jobs in the long format by default.
setopt NOTIFY             # Report status of background jobs immediately.
# unsetopt BG_NICE          # Don't run all background jobs at a lower priority.
# unsetopt CHECK_JOBS       # Don't report on jobs when shell exit.
# unsetopt HUP              # Don't kill jobs on shell exit.

##: }}


##: HISTORY {{

# setopt APPEND_HISTORY            # Appends history to history file on exit
# setopt BANG_HIST                 # Don't treat '!' specially during expansion.
# setopt EXTENDED_HISTORY          # Write the history file in the ':start:elapsed;command' format.
# setopt HIST_BEEP                 # Beep when accessing non-existent history.
# setopt HIST_EXPIRE_DUPS_FIRST    # Expire a duplicate event first when trimming history.
# setopt HIST_FIND_NO_DUPS         # Do not display a previously found event.
# setopt HIST_IGNORE_ALL_DUPS      # Delete an old recorded event if a new event is a duplicate.
# setopt HIST_IGNORE_DUPS          # Do not record an event that was just recorded again.
# setopt HIST_IGNORE_SPACE         # Do not record an event starting with a space.
# setopt HIST_SAVE_NO_DUPS         # Do not write a duplicate event to the history file.
# setopt HIST_VERIFY               # Do not execute immediately upon history expansion.
# setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
# setopt SHARE_HISTORY             # Share history between all sessions.

##: }}
