{
  e = "$EDITOR";

  mkdir = "mkdir -pv";

  # Use Kitty terminal"s ssh helper kitten
  sshk = "kitty +kitten ssh -o SendEnv=DOTFIELD_OS_APPEARANCE -A";
  # Display an image in kitty
  icat = "kitty +kitten icat";

  # Always enable colored `grep` output
  # Note: `GREP_OPTIONS="--color=auto"` is deprecated, hence the alias usage.
  grep = "grep --color=auto";
  fgrep = "fgrep --color=auto";
  egrep = "egrep --color=auto";

  xat = "hexyl";

  ".." = "cd ..";
  "..." = "cd ../..";
  "...." = "cd ../../..";
  "....." = "cd ../../../..";
  "......" = "cd ../../../../..";

  tree = "exa --tree";

  # IP addresses
  ip = "dig +short myip.opendns.com @resolver1.opendns.com";
  localip = "ipconfig getifaddr en1";

  # Flush DNS cache
  flushdns = "dscacheutil -flushcache";
}
