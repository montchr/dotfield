#!/usr/bin/env zsh

# Handle $0 according to the ZSH Plugin Standard:
# https://zdharma-continuum.github.io/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html
0="${${ZERO:-${0:#$ZSH_ARGZERO}}:-${(%):-%N}}"
0="${${(M)0:#/*}:-$PWD/$0}"

() {
  # Load all of the files in rc.d that start with <number>- and end in `.zsh`.
  # <https://zsh.sourceforge.io/Doc/Release/Expansion.html#Glob-Operators>
  # - `(n)`   => sort the results numerically
  # - `<->`   => open-ended range, matches any non-negative integer
  # - `<1->`  => matches any integer >= 1
  # - `<-9>`  => matches any integer <= 9
  # - `<1-9>` => matches any integer that's >= 1 and <= 9
  local file=
  for file in ${DOTFIELD_USER_ZDOTDIR}/rc.d/<->-*.zsh(n); do
    # shellcheck source=/dev/null
    . $file   # `.` is like `source`, but doesn't search your $path.
  done
} "$@"
