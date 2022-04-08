{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.lib.our) dotfieldPath;
  inherit (config.xdg) configHome dataHome stateHome;

  configPath = "${configHome}/dotfield/config";
  chemacsDir = "${configHome}/emacs";

  doomProfilePath = "emacs/profiles/doom";
  vanillaProfilePath = "emacs/profiles/vanilla";
  xtallosProfilePath = "emacs/profiles/xtallos";

  # Note that this points to the doom config directory within the flake source.
  # Consider changing if you run into issues.
  doomDir = "${configPath}/${doomProfilePath}";

  doomDataDir = "${dataHome}/${doomProfilePath}";
  doomStateDir = "${stateHome}/${doomProfilePath}";
in {
  home.sessionPath = ["${doomDataDir}/bin" "$PATH"];

  home.sessionVariables = {
    EDITOR = "emacsclient";

    ## doom-emacs
    # user config files
    DOOMDIR = doomDir;
    # local state :: `$DOOMDIR/.local` by default.
    DOOMLOCALDIR = doomStateDir;
    # source
    EMACSDIR = doomDataDir;

    # lsp: use plists instead of hashtables for performance improvement
    # https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
    LSP_USE_PLISTS = "true";
  };

  xdg.configFile = {
    # chemacs source :: must be installed to $EMACSDIR
    "emacs" = {
      source = pkgs.sources.chemacs.src;
      recursive = true;
    };

    # chemacs config
    "chemacs/profiles.el".text = ''
      (("default" . ((user-emacs-directory . "${doomDataDir}")))
       ("vanilla" . ((user-emacs-directory . "${configPath}/${vanillaProfilePath}")))
       ("xtallos" . ((user-emacs-directory . "${configPath}/${xtallosProfilePath}"))))
    '';

    # chemacs default profile :: will load when no `--with-profile` is provided
    "chemacs/profile".text = "default";
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGcc;
  };

  home.packages = with pkgs; [
    dotfield.ediffTool

    ## Doom dependencies
    (ripgrep.override {withPCRE2 = true;})
    gnutls

    ## Optional dependencies
    fd # faster projectile indexing
    imagemagick # for image-dired
    zstd # for undo-fu-session/undo-tree compression
    pinentry_emacs

    ## Module dependencies
    # :checkers spell
    (aspellWithDicts (ds: with ds; [en en-computers en-science]))

    # :checkers grammar
    languagetool

    # :term vterm
    # "Couldn't find cmake command. Vterm module won't compile"
    # -- Probably unnecessary, but okay.
    cmake

    # :tools editorconfig
    editorconfig-core-c

    # :tools docker
    nodePackages.dockerfile-language-server-nodejs

    # :tools lookup
    # & :lang org +roam
    sqlite

    # :tools terraform
    terraform
    terraform-ls

    # :lang css
    nodePackages.vscode-css-languageserver-bin

    # :lang javascript
    nodePackages.eslint
    # eslint, but as a service
    # https://github.com/mantoni/eslint_d.js
    nodePackages.eslint_d
    nodePackages.typescript-language-server

    # :lang json
    nodePackages.vscode-json-languageserver

    # :lang ledger
    ledger

    # :lang markdown
    nodePackages.unified-language-server

    # :lang nix
    nixpkgs-fmt
    rnix-lsp

    # :lang org
    graphviz

    # :lang php
    nodePackages.intelephense

    # :lang python
    (python3.withPackages (ps:
      with ps; [
        black
        grip
        pip
        pylint
        # FIXME: this package is "broken" on some systems?
        # python-lsp-server
        setuptools
      ]))

    # :lang ruby
    rubyPackages.solargraph

    # :lang sh
    nodePackages.bash-language-server

    # :lang toml
    taplo-lsp

    # :lang web
    nodePackages.js-beautify
    nodePackages.stylelint
    nodePackages.vscode-html-languageserver-bin

    # :lang yaml
    nodePackages.yaml-language-server

    ## Additional modes {{{

    # vimrc-mode
    nodePackages.vim-language-server

    ## }}}
  ];
}
