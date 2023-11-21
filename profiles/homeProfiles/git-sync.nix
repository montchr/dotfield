{
  config,
  lib,
  ...
}: let
  inherit (config.home) homeDirectory;
  base = "${homeDirectory}/Developer";
  mirrors = "${base}/mirrors";
  repo = path: uri:
    lib.nameValuePair path {
      inherit uri;
      path = "${mirrors}/${path}";
    };
  gh = path: repo path "git@github.com:${path}.git";
in {
  services.git-sync.repositories = lib.listToAttrs [
    (gh "doomemacs/doomemacs")
    (gh "purcell/emacs.d")
    (gh "bbatsov/prelude")
    (gh "noctuid/general.el")
  ];
}
