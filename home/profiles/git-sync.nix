{ config, lib, ... }:
let
  inherit (config.home) homeDirectory;
  base = "${homeDirectory}/Projects";
  mirrors = "${base}/mirrors";
  repo =
    path: uri:
    lib.nameValuePair path {
      inherit uri;
      path = "${mirrors}/${path}";
    };
  gh = path: repo path "git@github.com:${path}.git";
  srht = path: repo path "git@git.sr.ht:~${path}";
in
{
  services.git-sync.enable = true;
  services.git-sync.repositories = lib.listToAttrs [
    (gh "doomemacs/doomemacs")
    (gh "purcell/emacs.d")
    (gh "bbatsov/prelude")
    (gh "noctuid/general.el")
    # TODO: reduce interval
    # (gh "nixos/nixpkgs")
    (gh "nix-community/home-manager")
    # TODO: remove (just here as test)
    (srht "montchr/password-store")
  ];
}
