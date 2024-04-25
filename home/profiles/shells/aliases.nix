{ pkgs }:
{
  ".." = "cd ..";
  "..." = "cd ../..";

  # FIXME: needs workaround for nushell
  # e = "$EDITOR";

  l = "${pkgs.eza}/bin/eza -bl --git --icons --time-style long-iso --group-directories-first";

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
  tree = "eza --tree";

  # IPs/DNS
  # FIXME: broken...? Quad9?
  getip = "curl ifconfig.me";
  # FIXME: depends on iface name
  # localip = "ipconfig getifaddr en1";
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
