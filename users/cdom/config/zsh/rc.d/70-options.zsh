#!/usr/bin/env zsh

### :: OPTIONS ::

# Treat these characters as part of a word.
WORDCHARS='_*?[]~&.;!#$%^(){}<>'


###: INTERACTION/FEEDBACK ======================================================

setopt COMBINING_CHARS         # Combine zero-length punc chars (accents) with base char
setopt HIST_BEEP               # Beep when accessing non-existent history.
setopt INTERACTIVE_COMMENTS    # Enable comments in interactive shell.
setopt RC_QUOTES               # Allow 'Henry''s Garage' instead of 'Henry'\''s Garage'

unsetopt BEEP                  # And I thought I *liked* beeping...
unsetopt BRACE_CCL             # Allow brace character class list expansion.
unsetopt MAIL_WARNING          # Don't print a warning message if a mail file has been accessed.

# TODO: annotate
setopt HASH_LIST_ALL
setopt IGNOREEOF
unsetopt CORRECT_ALL
unsetopt NOMATCH


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
