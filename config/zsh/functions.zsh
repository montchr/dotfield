# Autoload personal functions
if [[ -z ${fpath[(r)${ZDOTDIR}/functions]} ]] {
  fpath=("${ZDOTDIR}/functions" "${fpath[@]}")
}
autoload -Uz $fpath[1]/*(:t)

autoload -U zmv

# ls on cd
add-zsh-hook chpwd chpwd_ls

# Use chpwd_recent_dirs to start new sessions from last working dir
autoload -Uz chpwd_recent_dirs
add-zsh-hook chpwd chpwd_recent_dirs

# Populate dirstack with chpwd history
zstyle ':chpwd:*' recent-dirs-file "${ZSH_RECENT_DIRS_FILE}"
dirstack=($(awk -F"'" '{print $2}' ${$(zstyle -L ':chpwd:*' recent-dirs-file)[4]} 2>/dev/null))
[[ "${PWD}" == "${HOME}" || "${PWD}" == "." ]] && () {
  local dir
  for dir ($dirstack) {
    [[ -d "${dir}" ]] && { cd -q "${dir}"; break }
  }
} 2>/dev/null


# =============================================================================
#    =%  FILESYSTEM  %=
# =============================================================================


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
};
compdef take=mkdir


# =============================================================================
#    =%  COLOR  %=
# =============================================================================


#=====================================
# Print the current shell's color palette.
#
# Outputs:
#   Available colors
#=====================================
function color::palette() {
    local -a colors
    for i in {000..255}; do
        colors+=("%F{$i}$i%f")
    done
    print -cP $colors
}


#=====================================
# Print an escape sequence for a given color code.
#
# Usage:
#   color::print <color-code>
# Parameters:
#   Color code
# Outputs:
#   Escape sequence
#=====================================
function color::print() {
    local color="%F{$1}"
    echo -E ${(qqqq)${(%)color}}
}


# =============================================================================
#    =%  MISCELLANEOUS  %=
# =============================================================================

#=====================================
# Jump to the documentation for a zsh builtin.
#
# Parameters:
#   Symbol name
#=====================================
function zman() {
  PAGER="less -g -I -s '+/^       "$1"'" man zshall;
}

