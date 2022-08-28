{
  e = "$EDITOR";

  mkdir = "mkdir -pv";

  # Use Kitty terminal"s ssh helper kitten
  sshk = "kitty +kitten ssh";
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

  top = "btm";
  tree = "exa --tree";

  # IPs/DNS
  getip = "curl ifconfig.me";
  localip = "ipconfig getifaddr en1";
  flushdns = "dscacheutil -flushcache";
}
