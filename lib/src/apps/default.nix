moduleArgs @ {l, ...}:
l.mapAttrs (_: v: import v moduleArgs) {
  firefox = ./firefox;
  fzf = ./fzf;
  kitty = ./kitty;
}
