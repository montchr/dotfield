
# ls on cd
add-zsh-hook chpwd chpwd_ls

# Start new sessions from most recent working directory.
autoload -Uz chpwd_recent_dirs
add-zsh-hook chpwd chpwd_recent_dirs

# Populate dirstack with chpwd history.
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

# FIXME
# zicompdef rcp=rsync
# FIXME
# zicompdef take=mkdir


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
