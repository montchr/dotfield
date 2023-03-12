{
  e = "$EDITOR";

  # Make life a bit less stressful.
  mv = "mv -i";
  cp = "cp -i";
  rm = "rm -I";

  # Make dir what I mean.
  mkdir = "mkdir -pv";

  # Use Kitty terminal"s ssh helper kitten
  # TODO: move to kitty profile
  # FIXME: mirror `ssh` completions... or: override `ssh` directly?
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
  # FIXME: this command is darwin-only
  flushdns = "dscacheutil -flushcache";

  ##: --- nix ------------------------------------------------------------------

  n = "nix";
  nd = "nix develop";
  ns = "nix shell";
  nsn = "nix shell nixpkgs#";
  nb = "nix build";
  nbn = "nix build nixpkgs#";
  nf = "nix flake";

  nr = "nixos-rebuild --flake .";
  nrs = "nixos-rebuild --flake . switch";
  snr = "sudo nixos-rebuild --flake .";
  snrs = "sudo nixos-rebuild --flake . switch";

  hm = "home-manager --flake .";
  hms = "home-manager --flake . switch";
}
