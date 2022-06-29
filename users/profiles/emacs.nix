{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.lib.our) dotfieldPath;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit
    (config.xdg)
    configHome
    dataHome
    stateHome
    ;

  corePackage =
    if isDarwin
    then pkgs.emacsNativeComp
    else pkgs.emacsPgtkNativeComp;

  dotfieldConfigPath = "${configHome}/dotfield/config";
  chemacsDir = "${configHome}/emacs";
  chemacsProfile = "doom";

  doomProfilePath = "emacs/profiles/doom";
  vanillaProfilePath = "emacs/profiles/vanilla";
  xtallosProfilePath = "emacs/profiles/xtallos";

  doomDir = "${dotfieldConfigPath}/${doomProfilePath}";
  doomDataDir = "${dataHome}/${doomProfilePath}";
  doomStateDir = "${stateHome}/${doomProfilePath}";
in {
  home.sessionPath = ["${doomDataDir}/bin" "$PATH"];

  home.sessionVariables = {
    CHEMACS_PROFILE = chemacsProfile;

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
    ## chemacs loader
    #
    # must be installed in one of the locations emacs looks for its
    # configuration directory (i.e. either ~/.config/emacs or ~/.emacs.d).
    "emacs" = {
      source = pkgs.sources.chemacs.src;
      recursive = true;
    };

    ## chemacs config
    #
    # N.B. doom must know about its environment variables before launching, so
    # we specify them in the chemacs profiles config file.
    #
    # see https://github.com/plexus/chemacs2#doom-emacs
    "chemacs/profiles.el".text = ''
      (("doom" . ((user-emacs-directory . "${doomDataDir}")
                  (env . (("DOOMDIR" . "${doomDir}")
                          ("DOOMLOCALDIR" . "${doomStateDir}")
                          ("EMACSDIR" . "${doomDataDir}")))
                  (server-name . "doom")))
       ("vanilla" . ((user-emacs-directory . "${dotfieldConfigPath}/${vanillaProfilePath}")))
       ("xtallos" . ((user-emacs-directory . "${dotfieldConfigPath}/${xtallosProfilePath}"))))
    '';

    ## chemacs default profile
    #
    # as a fallback in case the `CHEMACS_PROFILE` environment variable is not
    # set and the `--with-profile` flag is not passed to emacs.
    "chemacs/profile".text = chemacsProfile;
  };

  programs.emacs = {
    enable = true;
    # TODO: consider wrapping the emacs package so it has access to dependencies
    # without cluttering home packages
    package = (pkgs.emacsPackagesFor corePackage).emacsWithPackages (epkgs: [
      epkgs.vterm
    ]);
  };

  services.emacs = lib.mkIf (!isDarwin) {
    enable = true;
    defaultEditor = lib.mkForce true;
    socketActivation.enable = true;
    # client.enable = true;
  };

  home.packages = with pkgs; [
    ediff-tool

    ## Doom dependencies
    gnutls
    # doom requires ripgrep with PCRE2 support. this is handled in an overlay.
    ripgrep

    ## Optional dependencies
    fd # faster projectile indexing
    imagemagick # for image-dired
    zstd # for undo-fu-session/undo-tree compression

    ## Module dependencies
    # :checkers spell
    (aspellWithDicts (ds: with ds; [en en-computers en-science]))

    # :checkers grammar
    languagetool

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
    nodePackages.typescript-language-server

    # :lang json
    nodePackages.vscode-json-languageserver

    # :lang ledger
    # FIXME: marked as broken upstream
    # ledger

    # :lang markdown
    nodePackages.unified-language-server

    # :lang nix
    rnix-lsp

    # :lang org
    graphviz

    # :lang php
    nodePackages.intelephense

    # :lang python
    pipenv
    (python3.withPackages (ps:
      with ps; [
        black
        grip
        nose
        pip
        poetry
        pylint
        pytest
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
