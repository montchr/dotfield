### $DOTFIELD_USER_ZDOTDIR/main.zsh :: entrypoint for user-specific zsh config

# Handle $0 according to the standard:
# https://zdharma-continuum.github.io/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html
0="${${ZERO:-${0:#$ZSH_ARGZERO}}:-${(%):-%N}}"
0="${${(M)0:#/*}:-$PWD/$0}"

. ${0:h}/options.zsh
. ${0:h}/functions.zsh
