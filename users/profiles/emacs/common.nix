moduleArgs @ {
  config,
  lib,
  pkgs,
  self,
  ...
}: let
  inherit (pkgs.stdenv) hostPlatform;
  inherit (config.xdg) configHome;
in {
  home.sessionVariables = {
    EMACSDIR = "${configHome}/emacs";

    # lsp: use plists instead of hashtables for performance improvement
    # https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
    LSP_USE_PLISTS = "true";
  };

  programs.emacs = {
    enable = true;
    package =
      if hostPlatform.isDarwin
      then self.packages.${hostPlatform.system}.emacs-plus
      else if (moduleArgs.osConfig.services.xserver.enable or false)
      then pkgs.emacsPgtkNativeComp
      else pkgs.emacsNativeComp;
    extraPackages = epkgs: with epkgs; [vterm];
  };

  services.emacs = lib.mkIf (!hostPlatform.isDarwin) {
    enable = true;
    defaultEditor = lib.mkForce true;
    socketActivation.enable = true;
  };

  home.packages = with pkgs; [
    ediff-tool
    gnutls
    (ripgrep.override {withPCRE2 = true;})

    fd # faster projectile indexing
    imagemagick # for image-dired
    zstd # for undo-fu-session/undo-tree compression

    #: org
    graphviz

    # FIXME: sqlite binary unusable in org-roam and forge even after supplying
    # them... so we let these packages compile the binary... something is
    # wrong...
    gcc
    sqlite

    editorconfig-core-c

    ##: === writing ===

    (aspellWithDicts (ds:
      with ds; [
        en
        en-computers
        en-science
      ]))
    languagetool

    ##: === lang/lsp ===

    #: docker
    nodePackages.dockerfile-language-server-nodejs
    #: terraform
    terraform
    terraform-ls
    #: css
    nodePackages.vscode-css-languageserver-bin
    #: js
    nodePackages.eslint
    nodePackages.typescript-language-server
    #: json
    nodePackages.vscode-json-languageserver
    #: ledger
    # FIXME: marked as broken upstream
    # ledger
    #: markdown
    nodePackages.unified-language-server
    #: nix
    rnix-lsp
    #: php
    nodePackages.intelephense
    #: python
    pipenv
    (python310.withPackages (ps:
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
    #: ruby
    rubyPackages.solargraph
    #: sh
    nodePackages.bash-language-server
    #: toml
    taplo-lsp
    #: web-mode
    nodePackages.js-beautify
    nodePackages.stylelint
    nodePackages.vscode-html-languageserver-bin
    #: yaml
    nodePackages.yaml-language-server
    #: vimrc
    nodePackages.vim-language-server
  ];
}
