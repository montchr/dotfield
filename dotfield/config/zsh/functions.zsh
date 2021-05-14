# Autoload personal functions
if [[ -z ${fpath[(r)${ZDOTDIR}/functions]} ]] {
  fpath=("${ZDOTDIR}/functions" "${fpath[@]}")
}
autoload -Uz $fpath[1]/*(:t)

autoload -U zmv

# ls on cd
add-zsh-hook chpwd chpwd_ls

#=====================================
# Wrapper for rsync that respects gitignore.
#=====================================
function rcp() {
  # -a = -rlptgoD
  #   -r = recursive
  #   -l = copy symlinks as symlinks
  #   -p = preserve permissions
  #   -t = preserve mtimes
  #   -g = preserve owning group
  #   -o = preserve owner
  # -z = use compression
  # -P = show progress on transferred file
  # -J = don't touch mtimes on symlinks (always errors)
  rsync -azPJ \
    --include=.git/ \
    --filter=':- .gitignore' \
    --filter=":- $XDG_CONFIG_HOME/git/ignore" \
    "$@"
}; compdef rcp=rsync


#=====================================
# Create a directory and cd into it.
#
# Parameters:
#   Directory name
#=====================================
function take() {
  mkdir -pv "$1" && cd "$1";
}; compdef take=mkdir

#=====================================
# Jump to the documentation for a zsh builtin.
#
# Parameters:
#   Symbol name
#=====================================
function zman() {
  PAGER="less -g -I -s '+/^       "$1"'" man zshall;
}

# Create a reminder with human-readable durations, e.g. 15m, 1h, 40s, etc
# TODO: make it work for darwin
# r() {
#   local time=$1; shift
#   sched "$time" "notify-send --urgency=critical 'Reminder' '$@'; ding";
# }; compdef r=sched
