# FIXME: this file is only necessary to pass args down -- find a better importer...
moduleArgs @ {l, ...}:
l.mapAttrs (_: v: import v moduleArgs) {
  firefox = ./firefox;
  fzf = ./fzf;
  kitty = ./kitty;
}
