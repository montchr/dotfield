function fn2comment -d "Copy a file's name to its Spotlight comment"
  osascript -e 'on run {f}' -e 'tell app "Finder" to set comment of (POSIX file f as alias) to f' -e end "$argv"
end
