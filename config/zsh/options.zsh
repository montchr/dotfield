###: GENERAL ===================================================================

# Treat these characters as part of a word.
# WORDCHARS='_-*?[]~&.;!#$%^(){}<>'

# ls et al.
# AUTO_LS_COMMANDS="exa --oneline"
# AUTO_LS_NEWLINE=false


###: INTERACTION/FEEDBACK ======================================================

setopt HASH_LIST_ALL
setopt GLOBDOTS                 # Glob dotfiles as well.
setopt COMBINING_CHARS          # Combine zero-length punc chars (accents) with base char
setopt EXTENDED_GLOB            # Enable more powerful glob features
setopt INTERACTIVE_COMMENTS     # Enable comments in interactive shell.
setopt RC_QUOTES                # Allow 'Henry''s Garage' instead of 'Henry'\''s Garage'

unsetopt CASE_GLOB        # Use case-insensitve globbing.


###: DIRECTORIES ===============================================================

setopt PUSHD_IGNORE_DUPS    # Don't push multiple copies of the same directory onto the directory stack.
setopt PUSHD_TO_HOME        # Push to home directory when no argument is given.

unsetopt AUTO_NAME_DIRS     # Don't add variable-stored paths to ~ list


###: PROCESSES ===============================================================

setopt AUTO_RESUME        # Attempt to resume existing job before creating a new process.
setopt LONG_LIST_JOBS     # List jobs in the long format by default.
setopt NOTIFY             # Report status of background jobs immediately.
