{
  config,
  lib,
  pkgs,
  flake,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (flake.perSystem.inputs') emacs-overlay nil-lsp;
  cfg = config.programs.emacs;
  sessionVariables = {
    # via <https://github.com/nix-community/home-manager/blob/80546b220e95a575c66c213af1b09fe255299438/modules/services/emacs.nix#L186C1-L191C11>
    EDITOR = lib.getBin (
      pkgs.writeShellScript "editor" ''
        exec ${lib.getBin cfg.package}/bin/emacsclient \
          "''${@:---create-frame}"
      ''
    );
  };
in
{
  imports = [
    # TODO: consolidate all extra non-elisp packages
    ./extra-packages.nix
  ];

  nixpkgs.overlays = [ flake.inputs.emacs-overlay.overlays.default ];

  home = {
    inherit sessionVariables;
  };
  programs.bash = {
    inherit sessionVariables;
  };
  programs.zsh = {
    inherit sessionVariables;
  };

  programs.emacs = {
    enable = true;
    package =
      if isDarwin then
        pkgs.emacs29-macport
      # else pkgs.emacs-pgtk; # from master via emacs-overlay
      # else pkgs.emacs29-pgtk;
      else
        emacs-overlay.packages.emacs-unstable-pgtk;
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
}
