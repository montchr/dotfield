{pkgs}: {
  ".." = "cd ..";
  "..." = "cd ../..";

  e = "$EDITOR";

  cp = "${pkgs.coreutils}/bin/cp -ir";
  # TODO: override `l` or `ll` from exa module
  le = "${pkgs.exa}/bin/exa -bl --git --icons --time-style long-iso --group-directories-first";
  mkdir = "${pkgs.coreutils}/bin/mkdir -pv";
  mv = "${pkgs.coreutils}/bin/mv -i";
  rm = "${pkgs.coreutils}/bin/rm -I";

  # Use Kitty terminal"s ssh helper kitten
  # TODO: move to kitty profile
  ssh = "kitty +kitten ssh";
  # Display an image in kitty
  icat = "kitty +kitten icat";

  # Always enable colored `grep` output
  # Note: `GREP_OPTIONS="--color=auto"` is deprecated, hence the alias usage.
  grep = "grep --color=auto";
  fgrep = "fgrep --color=auto";
  egrep = "egrep --color=auto";

  # Hashing.
  h256 = "sha256sum";
  h512 = "sha512sum";

  # Runs bat without line numbers and wrapping.
  rat = "bat --style=plain --wrap=never";

  top = "htop";
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

  ndr = "darwin-rebuild --flake .";
  nr = "nixos-rebuild --flake .";
  nrs = "nixos-rebuild --flake . switch";
  snr = "sudo nixos-rebuild --flake .";
  snrs = "sudo nixos-rebuild --flake . switch";

  hm = "home-manager --flake .";
  hms = "home-manager --flake . switch";
}
