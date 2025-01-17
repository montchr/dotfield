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

  # via <https://github.com/nix-community/home-manager/blob/80546b220e95a575c66c213af1b09fe255299438/modules/services/emacs.nix#L186C1-L191C11>
  editorPkg = pkgs.writeShellScriptBin "editor" ''
    exec ${lib.getBin cfg.package}/bin/emacsclient "''${@:---create-frame}"
  '';

  sessionVariables = {
    EDITOR = lib.getExe editorPkg;
  };
in
{
  imports = [
    # TODO: consolidate all extra non-elisp packages
    ./extra-packages.nix

    ../writing.nix
  ];

  # XXX: no effect!  <https://github.com/nix-community/home-manager/issues/2954#issuecomment-2185326566>
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
    # TODO: update darwin package to whatever it should be now
    package = if isDarwin then pkgs.emacs29-macport else emacs-overlay.packages.emacs-pgtk;
    extraPackages = epkgs: [
      (epkgs.jinx.override { enchant2 = pkgs.enchant; })
      epkgs.pdf-tools
      epkgs.ready-player
      epkgs.treesit-grammars.with-all-grammars
      epkgs.treesit-auto

      (pkgs.tree-sitter.buildGrammar {
        language = "phpdoc";
        version = "unstable-2024-05-06";
        src = pkgs.fetchFromGitHub {
          owner = "claytonrcarter";
          repo = "tree-sitter-phpdoc";
          rev = "1d0e255b37477d0ca46f1c9e9268c8fa76c0b3fc";
          sha256 = "sha256-EWj/Av8+Ri7KiC9LzH73ytufjkp3MxBPwfm6mF3IGD8=";
        };
      })

    ];
  };

  home.packages = [
    nil-lsp.packages.nil

    pkgs.fd
    pkgs.emacs-lsp-booster
    pkgs.enchant
    pkgs.nixd
    pkgs.ripgrep

    editorPkg
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
