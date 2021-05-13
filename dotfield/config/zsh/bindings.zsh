# -------------------------------------
#  GENERAL
# -------------------------------------

bindkey -v        # Vi bindings
bindkey "^A"      beginning-of-line     "^E"      end-of-line
bindkey "^?"      backward-delete-char  "^H"      backward-delete-char
bindkey "^W"      backward-kill-word    "\e[1~"   beginning-of-line
bindkey "\e[7~"   beginning-of-line     "\e[H"    beginning-of-line
bindkey "\e[4~"   end-of-line           "\e[8~"   end-of-line
bindkey "\e[F"    end-of-line           "\e[3~"   delete-char
bindkey "^J"      accept-line           "^M"      accept-line
bindkey "^T"      accept-line           "^R"      history-incremental-search-backward

# Do nothing on pageup and pagedown. Better than printing '~'.
bindkey -s '^[[5~' ''
bindkey -s '^[[6~' ''

# C-<space> to bypass completion
bindkey "^ " magic-space
# Normal space during searches
bindkey -M isearch " " magic-space

# history-substring-search
bindkey "$terminfo[kcuu1]" history-substring-search-up;
bindkey "$terminfo[kcud1]" history-substring-search-down;
bindkey "^[[A" history-substring-search-up;
bindkey "^[[B" history-substring-search-down;
bindkey -M vicmd "k" history-substring-search-up;
bindkey -M vicmd "j" history-substring-search-down;


# -------------------------------------
#  WIDGETS
# -------------------------------------

bindkey '^I' fzf-tab-partial-and-complete
# Expand aliases on <space>
bindkey " " globalias
