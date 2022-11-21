###: GENERAL ===================================================================

# Treat these characters as part of a word.
WORDCHARS='_-*?[]~&.;!#$%^(){}<>'

# ls et al.
AUTO_LS_COMMANDS="exa --oneline"
AUTO_LS_NEWLINE=false


###: COMPLETIONS ======================================================

setopt ALWAYS_TO_END     # Move cursor to the end of a completed word.
setopt AUTO_LIST         # Automatically list choices on ambiguous completion.
setopt AUTO_MENU         # Show completion menu on a successive tab press.
setopt AUTO_PARAM_SLASH  # If completed parameter is a directory, add a trailing slash.
setopt COMPLETE_IN_WORD  # Complete from both ends of a word.
setopt PATH_DIRS         # Perform path search even on command names with slashes.

unsetopt COMPLETE_ALIASES  # Disable completion of aliases
unsetopt MENU_COMPLETE     # Do not autoselect the first completion entry.


###: INTERACTION/FEEDBACK ======================================================

setopt COMBINING_CHARS         # Combine zero-length punc chars (accents) with base char
setopt HIST_BEEP               # Beep when accessing non-existent history.
setopt RC_QUOTES               # Allow 'Henry''s Garage' instead of 'Henry'\''s Garage'

unsetopt BEEP                  # And I thought I *liked* beeping...
unsetopt BRACE_CCL             # Allow brace character class list expansion.
unsetopt INTERACTIVE_COMMENTS  # Disable comments in interactive shell.
unsetopt MAIL_WARNING          # Don't print a warning message if a mail file has been accessed.

# TODO: annotate
setopt HASH_LIST_ALL
setopt IGNOREEOF
unsetopt CORRECT_ALL
unsetopt NOMATCH


###: DIRECTORIES ===============================================================

DIRSTACKSIZE=9

setopt AUTO_PUSHD           # Push the old directory onto the stack on cd.
setopt CDABLE_VARS          # Change directory to a path stored in a variable.
setopt EXTENDED_GLOB        # Enable more powerful glob features
setopt GLOBDOTS             # Glob dotfiles as well.
setopt MULTIOS              # Write to multiple descriptors.
setopt PUSHD_IGNORE_DUPS    # Don't push multiple copies of the same directory onto the directory stack.
setopt PUSHD_SILENT         # Do not print the directory stack after pushd or popd.
setopt PUSHD_TO_HOME        # Push to home directory when no argument is given.

unsetopt AUTO_NAME_DIRS     # Don't add variable-stored paths to ~ list
unsetopt CASE_GLOB          # Use case-insensitive globbing.


###: PROCESSES ===============================================================

setopt AUTO_RESUME        # Attempt to resume existing job before creating a new process.
setopt LONG_LIST_JOBS     # List jobs in the long format by default.
setopt NOTIFY             # Report status of background jobs immediately.

unsetopt BG_NICE          # Don't run all background jobs at a lower priority.
unsetopt CHECK_JOBS       # Don't report on jobs when shell exit.
unsetopt HUP              # Don't kill jobs on shell exit.

##: Summarise execution time (in seconds) for long-running commands.
REPORTTIME=2 # <- Threshold defining "long-running"
TIMEFMT="%U user %S system %P cpu %*Es total"
