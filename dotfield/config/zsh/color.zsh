
# TODO: grc looks nice but:
# 1. move this elsewhere
# 2. make it work with nix
#
# # grc colorizes the output of a lot of commands. If the user installed it,
# # use it.

# # Try and find the grc setup file
# if (( $+commands[grc] )); then
#   GRC_SETUP='/usr/local/etc/grc.bashrc'
# fi
# if (( $+commands[grc] )) && (( $+commands[brew] ))
# then
#   GRC_SETUP="$(brew --prefix)/etc/grc.bashrc"
# fi
# if [[ -r "$GRC_SETUP" ]]; then
#   source "$GRC_SETUP"
# fi
# unset GRC_SETUP

# if (( $+commands[grc] ))
# then
#   function ping5(){
#     grc --color=auto ping -c 5 "$@"
#   }
# else
#   alias ping5='ping -c 5'
# fi
