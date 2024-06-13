{ pkgs, flake, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (flake.inputs') emacs-overlay;
in
{
  nixpkgs.overlays = [ flake.inputs.emacs-overlay.overlays.default ];

  programs.emacs = {
    enable = true;
    # FIXME: per-user with emacs-unstable-pgtk as default
    package =
      if isDarwin then
        pkgs.emacs29-macport
      # else pkgs.emacs-pgtk; # from master via emacs-overlay
      # else pkgs.emacs29-pgtk;
      else
        emacs-overlay.packages.emacs-pgtk;
  };
}
