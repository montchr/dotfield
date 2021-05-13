# Autoload personal functions
if [[ -z ${fpath[(r)${ZDOTDIR}/functions]} ]] {
  fpath=("${ZDOTDIR}/functions" "${fpath[@]}")
}
autoload -Uz $fpath[1]/*(:t)

autoload -U zmv

# ls on cd
add-zsh-hook chpwd chpwd_ls
