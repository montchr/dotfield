# Long running processes should return time (in seconds) when they complete.
REPORTTIME=2
TIMEFMT="%U user %S system %P cpu %*Es total"

# Disable Oh-My-ZSH's internal updating.
export DISABLE_AUTO_UPDATE=true

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

export \
  ZSH_AUTOSUGGEST_USE_ASYNC=true \
  ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20 \
  ZSH_AUTOSUGGEST_HISTORY_IGNORE="?(#c100,)" \
  ZSH_AUTOSUGGEST_MANUAL_REBIND=set \
  ZSH_AUTOSUGGEST_STRATEGY=(history completion) \
  ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=(bracketed-paste) \
  ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=(autopair-insert)


# -------------------------------------
#  INTERACTION/FEEDBACK
# -------------------------------------

setopt HASH_LIST_ALL
setopt IGNOREEOF

setopt GLOBDOTS                 # Glob dotfiles as well.
setopt COMBINING_CHARS          # Combine zero-length punc chars (accents) with base char
setopt CORRECT                  # Enable corrections.
setopt EXTENDED_GLOB            # Enable more powerful glob features
setopt INTERACTIVE_COMMENTS     # Enable comments in interactive shell.
setopt PROMPT_SUBST             # Enable parameter expansion, command substitution, and arithmetic expansion in the prompt.
setopt RC_QUOTES                # Allow 'Henry''s Garage' instead of 'Henry'\''s Garage'

unsetopt BEEP             # No BEEP!
unsetopt BRACE_CCL        # Allow brace character class list expansion.
unsetopt CASE_GLOB        # Use case-insensitve globbing.
unsetopt CORRECT_ALL      # Turn off the infernal correctall for filenames.
unsetopt MAIL_WARNING     # Don't print a warning message if a mail file has been accessed.
unsetopt NOMATCH


# -------------------------------------
#  DIRECTORIES
# -------------------------------------

DIRSTACKSIZE=9

setopt AUTO_CD              # Auto changes to a directory without typing cd.
setopt AUTO_PUSHD           # Make cd push the old directory onto the directory stack
setopt CDABLE_VARS          # Change directory to a path stored in a variable.
setopt MULTIOS              # Perform implicit tees or cats when multiple redirections are attempted.
setopt PUSHD_IGNORE_DUPS    # Don't push multiple copies of the same directory onto the directory stack.
setopt PUSHD_SILENT         # Do not print the directory stack after pushd or popd.
setopt PUSHD_TO_HOME        # Push to home directory when no argument is given.
setopt PUSHDMINUS           # Swap the meaning of cd +1 and cd -1; we want them to mean the opposite of what they mean.

unsetopt GLOB_DOTS
unsetopt AUTO_NAME_DIRS     # Don't add variable-stored paths to ~ list


# -------------------------------------
#  HISTORY
# -------------------------------------

# setopt hist_reduce_blanks

setopt APPEND_HISTORY            # Appends history to history file on exit
setopt BANG_HIST                 # Don't treat '!' specially during expansion.
setopt EXTENDED_HISTORY          # Write the history file in the ':start:elapsed;command' format.
setopt HIST_BEEP                 # Beep when accessing non-existent history.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire a duplicate event first when trimming history.
setopt HIST_FIND_NO_DUPS         # Do not display a previously found event.
setopt HIST_IGNORE_ALL_DUPS      # Delete an old recorded event if a new event is a duplicate.
setopt HIST_IGNORE_DUPS          # Do not record an event that was just recorded again.
setopt HIST_IGNORE_SPACE         # Do not record an event starting with a space.
setopt HIST_SAVE_NO_DUPS         # Do not write a duplicate event to the history file.
setopt HIST_VERIFY               # Do not execute immediately upon history expansion.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.


# -------------------------------------
#  PROCESSES
# -------------------------------------

setopt AUTO_RESUME        # Attempt to resume existing job before creating a new process.
setopt LONG_LIST_JOBS     # List jobs in the long format by default.
setopt NOTIFY             # Report status of background jobs immediately.
unsetopt BG_NICE          # Don't run all background jobs at a lower priority.
unsetopt CHECK_JOBS       # Don't report on jobs when shell exit.
unsetopt HUP              # Don't kill jobs on shell exit.
