# Make it easy to copy/paste script commands verbatim
alias '$'=''

# Reload the shell (i.e. invoke as a login shell)
alias reload="exec $SHELL -l"


#=====================================
# Expand aliases inline.
#
# Similar to fish-shell abbreviations.
#
# http://blog.patshead.com/2012/11/automatically-expaning-zsh-global-aliases---simplified.html
#=====================================
function globalias() {
   if [[ $LBUFFER =~ ' [A-Z0-9]+$' ]]; then
     zle _expand_alias
     zle expand-word
   fi
   zle self-insert
}
# FIXME
# zle -N globalias
