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
  # FIXME:
  #   Jan 17 11:31:17 tuuvok hm-activate-cdom[177973]:     1: Failed load of wanted unit file /nix/store/k3144x89z3klczn17f807h7qiw6kyzfg-home-manager-generation/home-files/.config/systemd/user/default.target.wants/git-sync-doomemacs
  # Jan 17 11:31:17 tuuvok hm-activate-cdom[177973]:     2: Could not determine unit type from extension of "/nix/store/k3144x89z3klczn17f807h7qiw6kyzfg-home-manager-generation/home-files/.config/systemd/user/default.target.wants/git-sync-doomemacs"
  # services.git-sync.repositories = lib.listToAttrs [
  #   (gh "doomemacs/doomemacs")
  #   # TODO: reduce interval
  #   # (gh "nixos/nixpkgs")
  #   (gh "nix-community/home-manager")
  #   # TODO: remove (just here as test)
  #   (srht "montchr/password-store")
  # ];
}
