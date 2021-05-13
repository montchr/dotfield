# -------------------------------------
#  PATTERNS
# -------------------------------------

# Use case-insensitve globbing.
unsetopt case_glob
# glob dotfiles as well
setopt globdots
# Enable more powerful glob features
setopt extended_glob
# Allow brace character class list expansion.
setopt brace_ccl
# Combine zero-length punctuation characters (accents) with the base character.
setopt combining_chars
# Allow 'Henry''s Garage' instead of 'Henry'\''s Garage'.
setopt rc_quotes



# -------------------------------------
#  INTERACTION/FEEDBACK
# -------------------------------------

# Turn on corrections.
setopt correct
# Turn off the infernal correctall for filenames.
unsetopt correctall
# Enable parameter expansion, command substitution, and arithmetic expansion in the prompt.
setopt prompt_subst
# Enable comments in interactive shell.
setopt interactive_comments
# Don't print a warning message if a mail file has been accessed.
unsetopt mail_warning

# Don't beep on error.
# setopt no_beep
# Perform implicit tees or cats when multiple redirections are attempted.
# setopt multios


# -------------------------------------
#  DIRECTORIES
# -------------------------------------

# Automatically change directory if a directory is entered
setopt autocd
# Don't push multiple copies of the same directory onto the directory stack.
setopt pushd_ignore_dups
# Make cd push the old directory onto the directory stack
setopt auto_pushd
# Swap the meaning of cd +1 and cd -1; we want them to mean the opposite of what they mean.
setopt pushdminus


# -------------------------------------
#  HISTORY
# -------------------------------------

setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt hist_save_no_dups
setopt hist_verify
unsetopt hist_beep

# Share history between all sessions.
setopt share_history
#setopt noclobber


# -------------------------------------
#  PROCESSES
# -------------------------------------

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
# unsetopt check_jobs
