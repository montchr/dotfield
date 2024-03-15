{
  lib,
  pkgs,
  flake,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (flake.perSystem.inputs') emacs-overlay nil-lsp;
in
# inherit (config) xdg;
# inherit (config.lib.file) mkOutOfStoreSymlink;
{
  imports = [
    # TODO: consolidate all extra non-elisp packages
    ./extra-packages.nix
  ];

  # xdg.configFile."emacs".source = mkOutOfStoreSymlink "${xdg.configHome}/ceamx";

  programs.emacs = {
    enable = true;
    package =
      if isDarwin then
        pkgs.emacs29-macport
      # else pkgs.emacs-pgtk; # from master via emacs-overlay
      # else pkgs.emacs29-pgtk;
      else
        emacs-overlay.packages.emacs-unstable-pgtk; # 29.1.90
    extraPackages = epkgs: [
      epkgs.pdf-tools
      epkgs.treesit-grammars.with-all-grammars
      epkgs.treesit-auto
    ];
  };

  home.packages = [
    pkgs.fd
    pkgs.ripgrep
    nil-lsp.packages.nil
  ];

  xdg.mimeApps.defaultApplications = {
    "text/csv" = lib.singleton "emacs.desktop";
  };
}
