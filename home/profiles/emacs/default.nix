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

  # <https://mimetype.io/all-types>
  xdg.mimeApps.defaultApplications =
    let
      # TODO: there are definitely more filetypes...
      mimetypes = [
        "application/atom+xml"
        "application/davmount+xml"
        "application/ecmascript"
        "application/json"
        "application/pgp-encrypted"
        "application/pgp-signature"
        "application/rdf+xml"
        "application/x-latex"
        "application/x-sh"
        "application/x-shellscript"
        "application/x-tex"
        "application/x-tex-tfm"
        "application/x-texinfo"
        "application/xml"
        "application/xml-dtd"
        "application/yaml"
        "test/mimetype"
        "text/css"
        "text/html"
        "text/csv"
        "text/javascript"
        "text/json"
        "text/markdown"
        "text/plain"
        "text/tab-separated-values"
        "text/vnd.curl"
        "text/x-markdown"
        "text/x-python"
        "text/x-vcard"
        "text/org"
        "text/text"
      ];
    in
    lib.genAttrs mimetypes (_: lib.singleton "emacsclient.desktop");

  nixpkgs.overlays = [ flake.inputs.emacs-overlay.overlays.default ];
}
